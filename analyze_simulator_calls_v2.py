#!/usr/bin/env python3
"""
F# Simulator Module Call Tree Analyzer v2
Improved version that handles private functions and module dependencies correctly
"""

import re
import json
import os
from pathlib import Path
from collections import defaultdict
import networkx as nx
import matplotlib.pyplot as plt
from pyvis.network import Network
import argparse
from datetime import datetime
from typing import Dict, List, Tuple, Set, Optional

class FSharpParser:
    """Enhanced parser for F# source code to extract functions and their calls"""
    
    def __init__(self):
        # Patterns for function definitions (including private)
        self.func_def_patterns = [
            # let functionName ... =
            re.compile(r'^\s*let\s+rec\s+private\s+(\w+)', re.MULTILINE),
            re.compile(r'^\s*let\s+private\s+rec\s+(\w+)', re.MULTILINE),
            re.compile(r'^\s*let\s+private\s+(\w+)', re.MULTILINE),
            re.compile(r'^\s*let\s+rec\s+(\w+)', re.MULTILINE),
            re.compile(r'^\s*let\s+(\w+)', re.MULTILINE),
            # member this.functionName ... =
            re.compile(r'^\s*member\s+private\s+\w+\.(\w+)', re.MULTILINE),
            re.compile(r'^\s*member\s+\w+\.(\w+)', re.MULTILINE),
            # static member functionName ... =
            re.compile(r'^\s*static\s+member\s+private\s+(\w+)', re.MULTILINE),
            re.compile(r'^\s*static\s+member\s+(\w+)', re.MULTILINE),
            # val functionName : signature (in .fsi files or signatures)
            re.compile(r'^\s*val\s+private\s+(\w+)\s*:', re.MULTILINE),
            re.compile(r'^\s*val\s+(\w+)\s*:', re.MULTILINE),
        ]
        
        # Pattern to check if function is private
        self.private_pattern = re.compile(r'^\s*(?:let|member|static\s+member|val)\s+(?:rec\s+)?private\s+|^\s*let\s+private\s+rec\s+')
        
        # F# keywords to exclude
        self.keywords = {
            'let', 'rec', 'if', 'then', 'else', 'match', 'with', 'when', 'for', 'to', 'do',
            'while', 'try', 'finally', 'fun', 'function', 'type', 'of', 'module', 'open',
            'namespace', 'new', 'use', 'using', 'static', 'member', 'val', 'return', 'yield',
            'abstract', 'override', 'interface', 'inherit', 'true', 'false', 'null', 'unit',
            'async', 'and', 'or', 'not', 'in', 'as', 'assert', 'base', 'begin', 'class',
            'default', 'delegate', 'done', 'downcast', 'downto', 'elif', 'end', 'exception',
            'extern', 'fixed', 'inline', 'internal', 'lazy', 'mutable', 'private', 'public',
            'raise', 'select', 'struct', 'upcast', 'void', 'volatile', 'printfn', 'printf',
            'sprintf', 'fprintf', 'failwith', 'failwithf', 'Some', 'None', 'Ok', 'Error',
            'fst', 'snd', 'id', 'ignore', 'ref', 'incr', 'decr', 'Array', 'List', 'Map',
            'Set', 'Seq', 'Option', 'Result'
        }
        
        # Common F# types to exclude
        self.types = {
            'int', 'uint32', 'uint64', 'int64', 'float', 'double', 'string', 'bool', 'char',
            'byte', 'sbyte', 'int16', 'uint16', 'single', 'decimal', 'bigint', 'unit',
            'list', 'array', 'seq', 'option', 'result', 'map', 'dict', 'set'
        }

    def extract_module_name(self, content: str, file_path: Path) -> str:
        """Extract module name from F# file"""
        # First try to find explicit module declaration
        match = re.search(r'^\s*module\s+(\w+(?:\.\w+)*)', content, re.MULTILINE)
        if match:
            return match.group(1).split('.')[-1]  # Take last part of qualified name
        
        # If no explicit module, use file name
        return file_path.stem

    def extract_functions_with_visibility(self, content: str) -> List[Tuple[str, bool]]:
        """Extract all function definitions with visibility info (name, is_private)"""
        functions = []
        seen = set()
        
        lines = content.split('\n')
        for i, line in enumerate(lines):
            for pattern in self.func_def_patterns:
                match = pattern.match(line)
                if match:
                    func_name = match.group(1)
                    if func_name not in self.keywords and func_name not in self.types and func_name not in seen:
                        is_private = bool(self.private_pattern.match(line))
                        functions.append((func_name, is_private))
                        seen.add(func_name)
                        break
        
        return functions

    def extract_function_calls(self, content: str, line_start: int = 0) -> List[Tuple[str, int]]:
        """Extract function calls with line numbers"""
        calls = []
        lines = content.split('\n')
        
        for line_num, line in enumerate(lines[line_start:], start=line_start+1):
            # Skip comments
            if line.strip().startswith('//') or line.strip().startswith('(*'):
                continue
            
            # Remove string literals to avoid false positives
            line_without_strings = re.sub(r'"[^"]*"', '', line)
            
            # Direct function calls
            matches = re.findall(r'(?<![.\w])(\w+)\s*(?=[\(\[\{]|\s+[\w\(\[\{"\'])', line_without_strings)
            for match in matches:
                if match not in self.keywords and match not in self.types:
                    calls.append((match, line_num))
            
            # Piped functions
            pipe_matches = re.findall(r'\|>\s*(\w+)', line_without_strings)
            for match in pipe_matches:
                if match not in self.keywords and match not in self.types:
                    calls.append((match, line_num))
            
            # Function composition
            comp_matches = re.findall(r'>>\s*(\w+)', line_without_strings)
            for match in comp_matches:
                if match not in self.keywords and match not in self.types:
                    calls.append((match, line_num))
            
            # Module qualified calls
            module_matches = re.findall(r'(\w+)\.(\w+)', line_without_strings)
            for module_name, func_name in module_matches:
                if func_name not in self.keywords and func_name not in self.types:
                    calls.append((f"{module_name}.{func_name}", line_num))
        
        return calls

    def extract_function_body(self, content: str, func_name: str) -> Tuple[str, int]:
        """Extract function body and starting line"""
        lines = content.split('\n')
        
        # Find function definition
        for i, line in enumerate(lines):
            # Check for function definition (with or without private)
            if re.match(rf'^\s*let\s+(?:rec\s+)?(?:private\s+)?{func_name}\b', line) or \
               re.match(rf'^\s*let\s+private\s+rec\s+{func_name}\b', line) or \
               re.match(rf'^\s*member\s+(?:private\s+)?\w+\.{func_name}\b', line):
                
                # Extract function body based on indentation
                start_line = i
                body_lines = [line]
                base_indent = len(line) - len(line.lstrip())
                
                for j in range(i + 1, len(lines)):
                    next_line = lines[j]
                    if next_line.strip() == "":
                        body_lines.append(next_line)
                        continue
                    
                    # Check if we've reached the next definition at the same level
                    next_indent = len(next_line) - len(next_line.lstrip())
                    if next_indent <= base_indent and (
                        next_line.strip().startswith('let ') or
                        next_line.strip().startswith('type ') or
                        next_line.strip().startswith('module ') or
                        next_line.strip().startswith('member ') or
                        next_line.strip().startswith('val ')):
                        break
                    
                    body_lines.append(next_line)
                
                return ('\n'.join(body_lines), start_line)
        
        return ("", -1)

class SimulatorAnalyzer:
    """Enhanced analyzer for ISSIE Simulator module"""
    
    def __init__(self, simulator_path: str):
        self.simulator_path = Path(simulator_path)
        self.parser = FSharpParser()
        self.modules = {}
        self.function_calls = []
        self.call_graph = nx.DiGraph()
        self.private_functions = set()
        
    def analyze_file(self, file_path: Path) -> Dict:
        """Analyze a single F# file"""
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
        
        module_name = self.parser.extract_module_name(content, file_path)
        
        # Extract functions with visibility
        functions_with_vis = self.parser.extract_functions_with_visibility(content)
        functions = [f[0] for f in functions_with_vis]
        private_funcs = [f[0] for f in functions_with_vis if f[1]]
        
        # Track private functions globally
        for func in private_funcs:
            self.private_functions.add(f"{module_name}.{func}")
        
        # Analyze each function's calls
        function_calls = {}
        for func, _ in functions_with_vis:
            body, start_line = self.parser.extract_function_body(content, func)
            if body:
                calls = self.parser.extract_function_calls(body, start_line)
                function_calls[func] = calls
        
        return {
            'module': module_name,
            'file': str(file_path.relative_to(self.simulator_path)),
            'functions': functions,
            'private_functions': private_funcs,
            'function_calls': function_calls
        }
    
    def analyze_all_files(self):
        """Analyze all F# files in the Simulator directory"""
        fs_files = list(self.simulator_path.rglob("*.fs"))
        
        # First pass: collect all modules and functions
        for file_path in sorted(fs_files):
            file_info = self.analyze_file(file_path)
            self.modules[file_info['module']] = file_info
        
        # Second pass: build call relationships and dependencies
        for module_name, file_info in self.modules.items():
            # Track module dependencies
            dependencies = set()
            
            for func, calls in file_info['function_calls'].items():
                full_func_name = f"{module_name}.{func}"
                
                for called_func, line_num in calls:
                    # Handle module-qualified calls
                    if '.' in called_func:
                        parts = called_func.split('.')
                        if len(parts) == 2:
                            target_module, target_func = parts
                            if target_module in self.modules:
                                target_func_full = f"{target_module}.{target_func}"
                                dependencies.add(target_module)
                            else:
                                target_func_full = called_func
                        else:
                            target_func_full = called_func
                    else:
                        # First check if it's in the same module
                        if called_func in file_info['functions']:
                            target_func_full = f"{module_name}.{called_func}"
                        else:
                            # Look for the function in other modules
                            target_func_full = self.find_function_module(called_func)
                            if target_func_full:
                                target_module = target_func_full.split('.')[0]
                                if target_module != module_name:
                                    dependencies.add(target_module)
                    
                    if target_func_full:
                        self.function_calls.append({
                            'from': full_func_name,
                            'to': target_func_full,
                            'file': file_info['file'],
                            'line': line_num
                        })
                        self.call_graph.add_edge(full_func_name, target_func_full)
            
            # Store dependencies
            file_info['dependencies'] = sorted(list(dependencies))
    
    def find_function_module(self, func_name: str) -> Optional[str]:
        """Find which module contains a function"""
        for module_name, module_info in self.modules.items():
            if func_name in module_info['functions']:
                return f"{module_name}.{func_name}"
        return None
    
    def get_call_tree_json(self) -> Dict:
        """Generate JSON structure for call tree"""
        modules_data = {}
        for module_name, module_info in self.modules.items():
            modules_data[module_name] = {
                'file': module_info['file'],
                'functions': module_info['functions'],
                'private_functions': module_info.get('private_functions', []),
                'dependencies': module_info.get('dependencies', [])
            }
        
        return {
            'modules': modules_data,
            'calls': self.function_calls,
            'private_functions': list(self.private_functions)
        }
    
    def generate_visualization(self, output_file: str = "simulator_call_tree.html", include_private: bool = True):
        """Generate interactive visualization using pyvis"""
        net = Network(height="1000px", width="100%", directed=True, 
                      bgcolor="#222222", font_color="white")
        
        # Color scheme for different modules
        module_colors = {
            'Simulator': '#ff6b6b',
            'FastRun': '#4ecdc4',
            'FastCreate': '#45b7d1',
            'FastReduce': '#96ceb4',
            'FastReduceTT': '#ffeaa7',
            'FastExtract': '#dfe6e9',
            'GraphBuilder': '#74b9ff',
            'GraphMerger': '#a29bfe',
            'CanvasExtractor': '#fd79a8',
            'CanvasStateAnalyser': '#e17055',
            'SimulationGraphAnalyser': '#fdcb6e',
            'NumberHelpers': '#6c5ce7',
            'SimGraphTypes': '#00b894',
            'SimTypes': '#00cec9',
            'SynchronousUtils': '#b2bec3',
            'TruthTableTypes': '#636e72',
            'BiBits': '#747d8c'
        }
        
        # Add nodes
        for node in self.call_graph.nodes():
            if not include_private and node in self.private_functions:
                continue
                
            module = node.split('.')[0]
            color = module_colors.get(module, '#ffffff')
            
            # Determine if private
            is_private = node in self.private_functions
            shape = "diamond" if is_private else "dot"
            
            # Highlight important functions
            important_funcs = ['startCircuitSimulation', 'startCircuitSimulationFData', 
                             'buildFastSimulation', 'stepSimulation', 'fastReduce', 
                             'runFastSimulation', 'validateCircuitSimulation']
            
            func_name = node.split('.')[-1]
            if any(node.endswith(f) for f in important_funcs):
                net.add_node(node, label=func_name, color=color, size=30, 
                           title=f"{'PRIVATE ' if is_private else ''}Module: {module}", 
                           shape="star" if not is_private else "triangle")
            else:
                net.add_node(node, label=func_name, color=color, size=20 if is_private else 15,
                           title=f"{'PRIVATE ' if is_private else ''}{node}", shape=shape)
        
        # Add edges
        for edge in self.call_graph.edges():
            if not include_private and (edge[0] in self.private_functions or edge[1] in self.private_functions):
                continue
            net.add_edge(edge[0], edge[1])
        
        # Configure physics
        net.barnes_hut(gravity=-80000, central_gravity=0.3, spring_length=200, 
                       spring_strength=0.01, damping=0.09)
        
        # Add options for better interaction
        net.set_options("""
        var options = {
          "nodes": {
            "font": {
              "size": 12
            }
          },
          "edges": {
            "color": {
              "inherit": true
            },
            "smooth": {
              "type": "continuous"
            }
          },
          "physics": {
            "barnesHut": {
              "gravitationalConstant": -80000,
              "springConstant": 0.001,
              "springLength": 200
            }
          },
          "interaction": {
            "hover": true,
            "tooltipDelay": 200
          }
        }
        """)
        
        net.save_graph(output_file)
        print(f"Interactive visualization saved to: {output_file}")
    
    def generate_static_visualization(self, output_file: str = "simulator_call_tree.svg", layout: str = "spring"):
        """Generate static visualization using matplotlib"""
        plt.figure(figsize=(24, 20))
        
        # Choose layout
        if layout == "hierarchical":
            # Create subset mapping for multipartite layout
            subset_map = {}
            for node in self.call_graph.nodes():
                module = node.split('.')[0]
                if module not in subset_map:
                    subset_map[module] = []
                subset_map[module].append(node)
            
            # Assign subset attributes
            for i, (module, nodes) in enumerate(subset_map.items()):
                for node in nodes:
                    self.call_graph.nodes[node]['subset'] = i
            
            pos = nx.multipartite_layout(self.call_graph, subset_key='subset')
        elif layout == "circular":
            pos = nx.circular_layout(self.call_graph)
        else:
            pos = nx.spring_layout(self.call_graph, k=3, iterations=50)
        
        # Color nodes by module and shape by visibility
        node_colors = []
        node_shapes = []
        for node in self.call_graph.nodes():
            module = node.split('.')[0]
            color_map = {
                'Simulator': '#ff6b6b',
                'FastRun': '#4ecdc4',
                'FastCreate': '#45b7d1',
                'FastReduce': '#96ceb4',
                'FastReduceTT': '#ffeaa7',
                'GraphBuilder': '#74b9ff',
                'NumberHelpers': '#6c5ce7'
            }
            node_colors.append(color_map.get(module, '#cccccc'))
            
        # Draw nodes with different sizes for private/public
        public_nodes = [n for n in self.call_graph.nodes() if n not in self.private_functions]
        private_nodes = [n for n in self.call_graph.nodes() if n in self.private_functions]
        
        # Draw public nodes
        nx.draw_networkx_nodes(self.call_graph, pos, nodelist=public_nodes,
                              node_color=[node_colors[i] for i, n in enumerate(self.call_graph.nodes()) if n in public_nodes],
                              node_size=300, node_shape='o')
        
        # Draw private nodes with different shape
        nx.draw_networkx_nodes(self.call_graph, pos, nodelist=private_nodes,
                              node_color=[node_colors[i] for i, n in enumerate(self.call_graph.nodes()) if n in private_nodes],
                              node_size=200, node_shape='s', alpha=0.7)
        
        # Draw edges
        nx.draw_networkx_edges(self.call_graph, pos, edge_color='gray', arrows=True, 
                              arrowsize=10, alpha=0.5, arrowstyle='->')
        
        # Draw labels
        labels = {node: node.split('.')[-1] for node in self.call_graph.nodes()}
        nx.draw_networkx_labels(self.call_graph, pos, labels, font_size=8, font_weight='bold')
        
        plt.title("ISSIE Simulator Module Call Tree\n(Squares = Private Functions)", fontsize=16)
        plt.axis('off')
        plt.tight_layout()
        plt.savefig(output_file, format='svg', dpi=300, bbox_inches='tight')
        print(f"Static visualization saved to: {output_file}")
    
    def generate_stats(self) -> Dict:
        """Generate statistics about the call graph"""
        stats = {
            'total_modules': len(self.modules),
            'total_functions': sum(len(m['functions']) for m in self.modules.values()),
            'total_public_functions': sum(len(m['functions']) - len(m.get('private_functions', [])) for m in self.modules.values()),
            'total_private_functions': len(self.private_functions),
            'total_calls': len(self.function_calls),
            'most_called_functions': [],
            'most_calling_functions': [],
            'recursive_functions': [],
            'isolated_functions': [],
            'key_functions_status': {}
        }
        
        # Check key functions
        key_functions = ['startCircuitSimulation', 'buildFastSimulation', 'stepSimulation', 'fastReduce']
        for func in key_functions:
            found = False
            for module_name, module_info in self.modules.items():
                if func in module_info['functions']:
                    is_private = func in module_info.get('private_functions', [])
                    stats['key_functions_status'][func] = {
                        'found': True,
                        'module': module_name,
                        'private': is_private
                    }
                    found = True
                    break
            if not found:
                stats['key_functions_status'][func] = {'found': False}
        
        # Find most called functions
        in_degrees = dict(self.call_graph.in_degree())
        most_called = sorted(in_degrees.items(), key=lambda x: x[1], reverse=True)[:10]
        stats['most_called_functions'] = [{'function': f, 'calls': c, 'private': f in self.private_functions} 
                                         for f, c in most_called]
        
        # Find functions that make the most calls
        out_degrees = dict(self.call_graph.out_degree())
        most_calling = sorted(out_degrees.items(), key=lambda x: x[1], reverse=True)[:10]
        stats['most_calling_functions'] = [{'function': f, 'calls': c, 'private': f in self.private_functions} 
                                          for f, c in most_calling]
        
        # Find recursive functions
        for node in self.call_graph.nodes():
            if self.call_graph.has_edge(node, node):
                stats['recursive_functions'].append({'function': node, 'private': node in self.private_functions})
        
        # Find isolated functions (no incoming or outgoing calls)
        for node in self.call_graph.nodes():
            if self.call_graph.in_degree(node) == 0 and self.call_graph.out_degree(node) == 0:
                stats['isolated_functions'].append({'function': node, 'private': node in self.private_functions})
        
        return stats

def compare_with_documentation(analyzer: SimulatorAnalyzer, doc_path: str) -> Dict:
    """Compare analysis results with existing documentation"""
    # Read the documentation
    with open(doc_path, 'r', encoding='utf-8') as f:
        doc_content = f.read()
    
    differences = {
        'missing_in_code': [],
        'missing_in_doc': [],
        'visibility_differences': [],
        'module_differences': []
    }
    
    # Extract documented functions from markdown
    documented_functions = set()
    # Look for function names in various contexts
    func_patterns = [
        re.compile(r'`(\w+)`\s*[-:]'),  # `functionName` -
        re.compile(r'val\s+(\w+)\s*:'),  # val functionName :
        re.compile(r'let\s+(?:rec\s+)?(\w+)'),  # let functionName
        re.compile(r'#### (\w+)\s*\n'),  # #### functionName
    ]
    
    for pattern in func_patterns:
        for match in pattern.finditer(doc_content):
            func = match.group(1)
            if func and not func.isupper():  # Exclude constants
                documented_functions.add(func)
    
    # Get all actual functions (including private)
    actual_functions = set()
    for module_info in analyzer.modules.values():
        actual_functions.update(module_info['functions'])
    
    # Find differences
    differences['missing_in_code'] = sorted(documented_functions - actual_functions)
    differences['missing_in_doc'] = sorted(actual_functions - documented_functions)
    
    # Check visibility of documented functions
    for func in documented_functions & actual_functions:
        # Find which module has this function
        for module_name, module_info in analyzer.modules.items():
            if func in module_info['functions']:
                is_private = func in module_info.get('private_functions', [])
                if is_private and 'private' not in doc_content.lower():
                    differences['visibility_differences'].append({
                        'function': func,
                        'module': module_name,
                        'actual': 'private',
                        'documented': 'public'
                    })
                break
    
    return differences

def update_documentation(analyzer: SimulatorAnalyzer, doc_path: str, output_path: str):
    """Update the documentation with accurate information"""
    with open(doc_path, 'r', encoding='utf-8') as f:
        doc_lines = f.readlines()
    
    # Generate updated content
    updated_lines = []
    changelog = []
    
    # Update timestamp
    timestamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    
    i = 0
    while i < len(doc_lines):
        line = doc_lines[i]
        
        # Update function list section
        if "## 5. Complete Function List by File" in line:
            updated_lines.append(line)
            updated_lines.append(f"<!-- Updated by analyzer on {timestamp} -->\n")
            updated_lines.append("<!-- Note: Private functions are marked with (private) -->\n\n")
            
            # Generate updated function list
            for module_name, module_info in sorted(analyzer.modules.items(), 
                                                  key=lambda x: x[1]['file']):
                updated_lines.append(f"### {module_info['file']}\n")
                updated_lines.append(f"**Module**: `{module_name}`\n")
                
                # Add module dependencies if any
                if module_info.get('dependencies'):
                    updated_lines.append(f"**Dependencies**: {', '.join(module_info['dependencies'])}\n")
                
                updated_lines.append("\n")
                
                # List functions
                for func in sorted(module_info['functions']):
                    is_private = func in module_info.get('private_functions', [])
                    visibility = " (private)" if is_private else ""
                    
                    # Get function calls
                    calls = []
                    for call in analyzer.function_calls:
                        if call['from'] == f"{module_name}.{func}":
                            target_func = call['to'].split('.')[-1]
                            if target_func not in calls:
                                calls.append(target_func)
                    
                    updated_lines.append(f"- `{func}{visibility}`")
                    if calls:
                        updated_lines.append(f" - Calls: {', '.join(calls[:5])}")
                        if len(calls) > 5:
                            updated_lines.append(f" (+{len(calls)-5} more)")
                    updated_lines.append("\n")
                
                updated_lines.append("\n")
            
            # Skip the original content until next major section
            i += 1
            while i < len(doc_lines) and not doc_lines[i].startswith("## 6."):
                i += 1
            i -= 1
            
            changelog.append(f"Updated function list with {sum(len(m['functions']) for m in analyzer.modules.values())} functions including private functions")
        
        # Update module dependency diagram
        elif "## 4. Module Call Relationship Diagram" in line:
            updated_lines.append(line)
            updated_lines.append(f"<!-- Updated by analyzer on {timestamp} -->\n")
            updated_lines.append("\n```mermaid\n")
            updated_lines.append("graph TD\n")
            
            # Generate updated mermaid diagram
            edges_added = set()
            for module_name, module_info in analyzer.modules.items():
                for dep in module_info.get('dependencies', []):
                    if dep in analyzer.modules and (module_name, dep) not in edges_added:
                        updated_lines.append(f"    {module_name} --> {dep}\n")
                        edges_added.add((module_name, dep))
            
            # Add styling for modules
            updated_lines.append("\n    %% Module styling\n")
            for module in analyzer.modules:
                if module == 'Simulator':
                    updated_lines.append(f"    style {module} fill:#ff9999\n")
                elif 'Fast' in module:
                    updated_lines.append(f"    style {module} fill:#99ff99\n")
                elif 'Graph' in module:
                    updated_lines.append(f"    style {module} fill:#9999ff\n")
                elif 'Canvas' in module:
                    updated_lines.append(f"    style {module} fill:#ffcc99\n")
                else:
                    updated_lines.append(f"    style {module} fill:#cccccc\n")
            
            updated_lines.append("```\n\n")
            
            # Skip original diagram
            i += 1
            while i < len(doc_lines) and not doc_lines[i].strip().startswith("```mermaid"):
                i += 1
            if i < len(doc_lines):
                while i < len(doc_lines) and not doc_lines[i].strip() == "```":
                    i += 1
                i += 1
            
            changelog.append("Updated module dependency diagram based on actual code analysis")
        
        # Update key functions status
        elif "### Main Simulation API" in line:
            updated_lines.append(line)
            stats = analyzer.generate_stats()
            
            # Add note about key functions
            updated_lines.append(f"\n<!-- Key Functions Status (Updated {timestamp}) -->\n")
            updated_lines.append("**Key Functions Verification:**\n")
            for func, status in stats['key_functions_status'].items():
                if status['found']:
                    visibility = "private" if status['private'] else "public"
                    updated_lines.append(f"- ✓ `{func}` - Found in {status['module']} ({visibility})\n")
                else:
                    updated_lines.append(f"- ✗ `{func}` - Not found in codebase\n")
            updated_lines.append("\n")
            
            changelog.append("Added key functions verification status")
        
        else:
            updated_lines.append(line)
        
        i += 1
    
    # Write updated documentation
    with open(output_path, 'w', encoding='utf-8') as f:
        f.writelines(updated_lines)
    
    # Generate detailed changelog
    changelog_path = output_path.replace('.md', '_changelog.md')
    with open(changelog_path, 'w', encoding='utf-8') as f:
        f.write(f"# Documentation Update Changelog\n\n")
        f.write(f"**Generated on**: {timestamp}\n\n")
        f.write("## Summary of Changes:\n\n")
        for change in changelog:
            f.write(f"- {change}\n")
        
        f.write("\n## Detailed Statistics:\n\n")
        stats = analyzer.generate_stats()
        f.write(f"- Total modules analyzed: {stats['total_modules']}\n")
        f.write(f"- Total functions found: {stats['total_functions']}\n")
        f.write(f"- Public functions: {stats['total_public_functions']}\n")
        f.write(f"- Private functions: {stats['total_private_functions']}\n")
        f.write(f"- Total function calls tracked: {stats['total_calls']}\n")
        
        f.write("\n## Key Functions Status:\n\n")
        for func, status in stats['key_functions_status'].items():
            if status['found']:
                f.write(f"- `{func}`: Found in {status['module']} ({'private' if status['private'] else 'public'})\n")
            else:
                f.write(f"- `{func}`: NOT FOUND\n")
    
    print(f"Updated documentation saved to: {output_path}")
    print(f"Changelog saved to: {changelog_path}")

def main():
    parser = argparse.ArgumentParser(description="Analyze ISSIE Simulator module function calls (v2)")
    parser.add_argument('--simulator-path', default='src/Renderer/Simulator',
                       help='Path to Simulator directory')
    parser.add_argument('--output-dir', default='.',
                       help='Output directory for generated files')
    parser.add_argument('--doc-path', default='SIMULATOR_API_DOCUMENTATION.md',
                       help='Path to existing documentation')
    parser.add_argument('--include-private', action='store_true',
                       help='Include private functions in visualization')
    
    args = parser.parse_args()
    
    # Create analyzer
    print("Analyzing ISSIE Simulator module (v2 - with private function support)...")
    analyzer = SimulatorAnalyzer(args.simulator_path)
    analyzer.analyze_all_files()
    
    # Generate outputs
    output_dir = Path(args.output_dir)
    output_dir.mkdir(exist_ok=True)
    
    # Save call tree JSON
    call_tree_json = analyzer.get_call_tree_json()
    json_path = output_dir / "simulator_call_tree_v2.json"
    with open(json_path, 'w', encoding='utf-8') as f:
        json.dump(call_tree_json, f, indent=2)
    print(f"Call tree JSON saved to: {json_path}")
    
    # Generate visualizations
    analyzer.generate_visualization(str(output_dir / "simulator_call_tree_v2.html"), 
                                   include_private=args.include_private)
    analyzer.generate_static_visualization(str(output_dir / "simulator_call_tree_v2.svg"))
    
    # Generate statistics
    stats = analyzer.generate_stats()
    stats_path = output_dir / "simulator_stats_v2.json"
    with open(stats_path, 'w', encoding='utf-8') as f:
        json.dump(stats, f, indent=2)
    print(f"Statistics saved to: {stats_path}")
    
    # Compare with documentation
    if os.path.exists(args.doc_path):
        print("\nComparing with existing documentation...")
        differences = compare_with_documentation(analyzer, args.doc_path)
        
        diff_path = output_dir / "documentation_differences_v2.json"
        with open(diff_path, 'w', encoding='utf-8') as f:
            json.dump(differences, f, indent=2)
        print(f"Differences saved to: {diff_path}")
        
        # Update documentation
        updated_doc_path = output_dir / "SIMULATOR_API_DOCUMENTATION_updated.md"
        update_documentation(analyzer, args.doc_path, str(updated_doc_path))
    
    # Print summary
    print("\n=== Analysis Summary ===")
    print(f"Total modules analyzed: {stats['total_modules']}")
    print(f"Total functions found: {stats['total_functions']}")
    print(f"  - Public functions: {stats['total_public_functions']}")
    print(f"  - Private functions: {stats['total_private_functions']}")
    print(f"Total function calls: {stats['total_calls']}")
    
    print(f"\nKey Functions Status:")
    for func, status in stats['key_functions_status'].items():
        if status['found']:
            print(f"  ✓ {func}: Found in {status['module']} ({'private' if status['private'] else 'public'})")
        else:
            print(f"  ✗ {func}: NOT FOUND")
    
    print(f"\nMost called functions:")
    for func_info in stats['most_called_functions'][:5]:
        private_marker = " [PRIVATE]" if func_info['private'] else ""
        print(f"  - {func_info['function']}{private_marker}: {func_info['calls']} calls")
    
    print(f"\nRecursive functions found: {len(stats['recursive_functions'])}")
    if stats['recursive_functions']:
        for func_info in stats['recursive_functions'][:5]:
            private_marker = " [PRIVATE]" if func_info['private'] else ""
            print(f"  - {func_info['function']}{private_marker}")

if __name__ == "__main__":
    main()