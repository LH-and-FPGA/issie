#!/usr/bin/env python3
"""
F# Simulator Module Call Tree Analyzer
Analyzes function call relationships in ISSIE's Simulator module
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
    """Parser for F# source code to extract functions and their calls"""
    
    def __init__(self):
        # Patterns for function definitions
        self.func_def_patterns = [
            # let functionName ... =
            re.compile(r'^\s*let\s+rec\s+(\w+)', re.MULTILINE),
            re.compile(r'^\s*let\s+(\w+)', re.MULTILINE),
            # member this.functionName ... =
            re.compile(r'^\s*member\s+\w+\.(\w+)', re.MULTILINE),
            # static member functionName ... =
            re.compile(r'^\s*static\s+member\s+(\w+)', re.MULTILINE),
            # val functionName : signature (in .fsi files or signatures)
            re.compile(r'^\s*val\s+(\w+)\s*:', re.MULTILINE),
        ]
        
        # Patterns for function calls
        self.func_call_patterns = [
            # Direct function call: functionName arg1 arg2
            re.compile(r'(?<![.\w])(\w+)\s+(?=[\w\(\[\{"])', re.MULTILINE),
            # Piped function: |> functionName
            re.compile(r'\|>\s*(\w+)', re.MULTILINE),
            # Function composition: >> functionName
            re.compile(r'>>\s*(\w+)', re.MULTILINE),
            # Module qualified: Module.functionName
            re.compile(r'(\w+)\.(\w+)\s*(?=[\w\(\[\{"])', re.MULTILINE),
            # Parenthesized call: (functionName arg)
            re.compile(r'\(\s*(\w+)\s+', re.MULTILINE),
        ]
        
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
            'fst', 'snd', 'id', 'ignore', 'ref', 'incr', 'decr'
        }
        
        # Common F# types to exclude
        self.types = {
            'int', 'uint32', 'uint64', 'int64', 'float', 'double', 'string', 'bool', 'char',
            'byte', 'sbyte', 'int16', 'uint16', 'single', 'decimal', 'bigint', 'unit',
            'list', 'array', 'seq', 'option', 'result', 'map', 'dict', 'set'
        }

    def extract_module_name(self, content: str) -> str:
        """Extract module name from F# file"""
        match = re.search(r'^\s*module\s+(\w+)', content, re.MULTILINE)
        if match:
            return match.group(1)
        return ""

    def extract_functions(self, content: str) -> List[str]:
        """Extract all function definitions from F# content"""
        functions = set()
        
        for pattern in self.func_def_patterns:
            matches = pattern.findall(content)
            for match in matches:
                if match not in self.keywords and match not in self.types:
                    functions.add(match)
        
        return sorted(list(functions))

    def extract_function_calls(self, content: str, line_start: int = 0) -> List[Tuple[str, int]]:
        """Extract function calls with line numbers"""
        calls = []
        lines = content.split('\n')
        
        for line_num, line in enumerate(lines[line_start:], start=line_start+1):
            # Skip comments
            if line.strip().startswith('//') or line.strip().startswith('(*'):
                continue
                
            # Direct function calls
            matches = re.findall(r'(?<![.\w])(\w+)\s+(?=[\w\(\[\{"])', line)
            for match in matches:
                if match not in self.keywords and match not in self.types:
                    calls.append((match, line_num))
            
            # Piped functions
            pipe_matches = re.findall(r'\|>\s*(\w+)', line)
            for match in pipe_matches:
                if match not in self.keywords and match not in self.types:
                    calls.append((match, line_num))
            
            # Module qualified calls
            module_matches = re.findall(r'(\w+)\.(\w+)', line)
            for module_name, func_name in module_matches:
                if func_name not in self.keywords and func_name not in self.types:
                    calls.append((f"{module_name}.{func_name}", line_num))
        
        return calls

    def extract_function_body(self, content: str, func_name: str) -> Tuple[str, int]:
        """Extract function body and starting line"""
        lines = content.split('\n')
        
        # Find function definition
        for i, line in enumerate(lines):
            # Check for function definition
            if re.match(rf'^\s*let\s+rec\s+{func_name}\b', line) or \
               re.match(rf'^\s*let\s+{func_name}\b', line) or \
               re.match(rf'^\s*member\s+\w+\.{func_name}\b', line):
                
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
                        next_line.strip().startswith('member ')):
                        break
                    
                    body_lines.append(next_line)
                
                return ('\n'.join(body_lines), start_line)
        
        return ("", -1)

class SimulatorAnalyzer:
    """Analyzer for ISSIE Simulator module"""
    
    def __init__(self, simulator_path: str):
        self.simulator_path = Path(simulator_path)
        self.parser = FSharpParser()
        self.modules = {}
        self.function_calls = []
        self.call_graph = nx.DiGraph()
        
    def analyze_file(self, file_path: Path) -> Dict:
        """Analyze a single F# file"""
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
        
        module_name = self.parser.extract_module_name(content)
        if not module_name:
            module_name = file_path.stem
        
        functions = self.parser.extract_functions(content)
        
        # Analyze each function's calls
        function_calls = {}
        for func in functions:
            body, start_line = self.parser.extract_function_body(content, func)
            if body:
                calls = self.parser.extract_function_calls(body, start_line)
                function_calls[func] = calls
        
        return {
            'module': module_name,
            'file': str(file_path.relative_to(self.simulator_path)),
            'functions': functions,
            'function_calls': function_calls
        }
    
    def analyze_all_files(self):
        """Analyze all F# files in the Simulator directory"""
        fs_files = list(self.simulator_path.rglob("*.fs"))
        
        for file_path in sorted(fs_files):
            file_info = self.analyze_file(file_path)
            self.modules[file_info['module']] = file_info
            
            # Build call relationships
            for func, calls in file_info['function_calls'].items():
                full_func_name = f"{file_info['module']}.{func}"
                
                for called_func, line_num in calls:
                    # Handle module-qualified calls
                    if '.' in called_func:
                        target_func = called_func
                    else:
                        # First check if it's in the same module
                        if called_func in file_info['functions']:
                            target_func = f"{file_info['module']}.{called_func}"
                        else:
                            # Look for the function in other modules
                            target_func = self.find_function_module(called_func)
                    
                    if target_func:
                        self.function_calls.append({
                            'from': full_func_name,
                            'to': target_func,
                            'file': file_info['file'],
                            'line': line_num
                        })
                        self.call_graph.add_edge(full_func_name, target_func)
    
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
            # Find module dependencies
            dependencies = set()
            for func_call in self.function_calls:
                if func_call['from'].startswith(f"{module_name}."):
                    target_module = func_call['to'].split('.')[0]
                    if target_module != module_name:
                        dependencies.add(target_module)
            
            modules_data[module_name] = {
                'file': module_info['file'],
                'functions': module_info['functions'],
                'dependencies': sorted(list(dependencies))
            }
        
        return {
            'modules': modules_data,
            'calls': self.function_calls
        }
    
    def generate_visualization(self, output_file: str = "simulator_call_tree.html"):
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
            'TruthTableTypes': '#636e72'
        }
        
        # Add nodes
        for node in self.call_graph.nodes():
            module = node.split('.')[0]
            color = module_colors.get(module, '#ffffff')
            
            # Highlight important functions
            if node.endswith('startCircuitSimulation') or \
               node.endswith('buildFastSimulation') or \
               node.endswith('stepSimulation') or \
               node.endswith('fastReduce'):
                net.add_node(node, label=node, color=color, size=30, 
                           title=f"Module: {module}", shape="star")
            else:
                net.add_node(node, label=node.split('.')[-1], color=color, size=15,
                           title=node, shape="dot")
        
        # Add edges
        for edge in self.call_graph.edges():
            net.add_edge(edge[0], edge[1])
        
        # Configure physics
        net.barnes_hut(gravity=-80000, central_gravity=0.3, spring_length=200, 
                       spring_strength=0.01, damping=0.09)
        
        net.save_graph(output_file)
        print(f"Interactive visualization saved to: {output_file}")
    
    def generate_static_visualization(self, output_file: str = "simulator_call_tree.svg"):
        """Generate static visualization using matplotlib"""
        plt.figure(figsize=(20, 16))
        
        # Use hierarchical layout
        pos = nx.spring_layout(self.call_graph, k=3, iterations=50)
        
        # Color nodes by module
        node_colors = []
        for node in self.call_graph.nodes():
            module = node.split('.')[0]
            color_map = {
                'Simulator': 'red',
                'FastRun': 'blue',
                'FastCreate': 'green',
                'FastReduce': 'orange',
                'GraphBuilder': 'purple',
                'NumberHelpers': 'brown'
            }
            node_colors.append(color_map.get(module, 'gray'))
        
        # Draw the graph
        nx.draw(self.call_graph, pos, node_color=node_colors, with_labels=True,
                node_size=100, font_size=8, arrows=True, edge_color='gray',
                arrowsize=10, font_weight='bold')
        
        plt.title("ISSIE Simulator Module Call Tree", fontsize=16)
        plt.axis('off')
        plt.tight_layout()
        plt.savefig(output_file, format='svg', dpi=300, bbox_inches='tight')
        print(f"Static visualization saved to: {output_file}")
    
    def generate_stats(self) -> Dict:
        """Generate statistics about the call graph"""
        stats = {
            'total_modules': len(self.modules),
            'total_functions': sum(len(m['functions']) for m in self.modules.values()),
            'total_calls': len(self.function_calls),
            'most_called_functions': [],
            'most_calling_functions': [],
            'recursive_functions': [],
            'isolated_functions': []
        }
        
        # Find most called functions
        in_degrees = dict(self.call_graph.in_degree())
        most_called = sorted(in_degrees.items(), key=lambda x: x[1], reverse=True)[:10]
        stats['most_called_functions'] = [{'function': f, 'calls': c} for f, c in most_called]
        
        # Find functions that make the most calls
        out_degrees = dict(self.call_graph.out_degree())
        most_calling = sorted(out_degrees.items(), key=lambda x: x[1], reverse=True)[:10]
        stats['most_calling_functions'] = [{'function': f, 'calls': c} for f, c in most_calling]
        
        # Find recursive functions
        for node in self.call_graph.nodes():
            if self.call_graph.has_edge(node, node):
                stats['recursive_functions'].append(node)
        
        # Find isolated functions (no incoming or outgoing calls)
        for node in self.call_graph.nodes():
            if self.call_graph.in_degree(node) == 0 and self.call_graph.out_degree(node) == 0:
                stats['isolated_functions'].append(node)
        
        return stats

def compare_with_documentation(analyzer: SimulatorAnalyzer, doc_path: str) -> Dict:
    """Compare analysis results with existing documentation"""
    # Read the documentation
    with open(doc_path, 'r', encoding='utf-8') as f:
        doc_content = f.read()
    
    differences = {
        'missing_in_code': [],
        'missing_in_doc': [],
        'signature_differences': []
    }
    
    # Extract documented functions from markdown
    documented_functions = set()
    func_pattern = re.compile(r'`(\w+)`|val\s+(\w+)\s*:|let\s+(\w+)')
    for match in func_pattern.finditer(doc_content):
        func = match.group(1) or match.group(2) or match.group(3)
        if func:
            documented_functions.add(func)
    
    # Get all actual functions
    actual_functions = set()
    for module_info in analyzer.modules.values():
        actual_functions.update(module_info['functions'])
    
    # Find differences
    differences['missing_in_code'] = sorted(documented_functions - actual_functions)
    differences['missing_in_doc'] = sorted(actual_functions - documented_functions)
    
    # Check for specific key functions mentioned in the task
    key_functions = ['startCircuitSimulation', 'buildFastSimulation', 'stepSimulation', 'fastReduce']
    for func in key_functions:
        if func not in actual_functions:
            print(f"WARNING: Key function '{func}' not found in code!")
    
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
    
    for i, line in enumerate(doc_lines):
        # Add update markers for modified sections
        if "## 5. Complete Function List by File" in line:
            updated_lines.append(line)
            updated_lines.append(f"<!-- Updated by analyzer on {timestamp} -->\n")
            
            # Generate updated function list
            for module_name, module_info in sorted(analyzer.modules.items()):
                updated_lines.append(f"\n### {module_info['file']}\n")
                updated_lines.append(f"**Module**: `{module_name}`\n")
                for func in module_info['functions']:
                    # Get function calls
                    calls = []
                    for call in analyzer.function_calls:
                        if call['from'] == f"{module_name}.{func}":
                            calls.append(call['to'].split('.')[-1])
                    
                    updated_lines.append(f"- `{func}` - ")
                    if calls:
                        updated_lines.append(f"Calls: {', '.join(set(calls)[:5])}\n")
                    else:
                        updated_lines.append("No direct calls\n")
            
            # Skip the original content until next major section
            while i < len(doc_lines) and not doc_lines[i].startswith("## 6."):
                i += 1
            i -= 1
            
            changelog.append(f"Updated function list based on code analysis")
        
        elif "## 4. Module Call Relationship Diagram" in line:
            updated_lines.append(line)
            updated_lines.append(f"<!-- Updated by analyzer on {timestamp} -->\n")
            updated_lines.append("\n```mermaid\n")
            updated_lines.append("graph TD\n")
            
            # Generate updated mermaid diagram
            module_deps = {}
            for module_name, module_info in analyzer.modules.items():
                module_deps[module_name] = module_info['dependencies']
            
            # Add nodes and edges
            for module, deps in module_deps.items():
                for dep in deps:
                    if dep in analyzer.modules:
                        updated_lines.append(f"    {module} --> {dep}\n")
            
            # Add styling
            updated_lines.append("    style Simulator fill:#ff9999\n")
            updated_lines.append("    style FastRun fill:#99ff99\n")
            updated_lines.append("    style SimGraphTypes fill:#9999ff\n")
            updated_lines.append("    style SimTypes fill:#9999ff\n")
            updated_lines.append("```\n")
            
            # Skip original diagram
            while i < len(doc_lines) and not "```" in doc_lines[i]:
                i += 1
            if i < len(doc_lines) and "```" in doc_lines[i]:
                i += 1
                while i < len(doc_lines) and not "```" in doc_lines[i]:
                    i += 1
            
            changelog.append("Updated module dependency diagram")
        else:
            updated_lines.append(line)
    
    # Write updated documentation
    with open(output_path, 'w', encoding='utf-8') as f:
        f.writelines(updated_lines)
    
    # Generate changelog
    changelog_path = output_path.replace('.md', '_changelog.md')
    with open(changelog_path, 'w', encoding='utf-8') as f:
        f.write(f"# Documentation Update Changelog\n\n")
        f.write(f"**Generated on**: {timestamp}\n\n")
        f.write("## Changes Made:\n\n")
        for change in changelog:
            f.write(f"- {change}\n")
    
    print(f"Updated documentation saved to: {output_path}")
    print(f"Changelog saved to: {changelog_path}")

def main():
    parser = argparse.ArgumentParser(description="Analyze ISSIE Simulator module function calls")
    parser.add_argument('--simulator-path', default='src/Renderer/Simulator',
                       help='Path to Simulator directory')
    parser.add_argument('--output-dir', default='.',
                       help='Output directory for generated files')
    parser.add_argument('--doc-path', default='SIMULATOR_API_DOCUMENTATION.md',
                       help='Path to existing documentation')
    
    args = parser.parse_args()
    
    # Create analyzer
    print("Analyzing ISSIE Simulator module...")
    analyzer = SimulatorAnalyzer(args.simulator_path)
    analyzer.analyze_all_files()
    
    # Generate outputs
    output_dir = Path(args.output_dir)
    output_dir.mkdir(exist_ok=True)
    
    # Save call tree JSON
    call_tree_json = analyzer.get_call_tree_json()
    json_path = output_dir / "simulator_call_tree.json"
    with open(json_path, 'w', encoding='utf-8') as f:
        json.dump(call_tree_json, f, indent=2)
    print(f"Call tree JSON saved to: {json_path}")
    
    # Generate visualizations
    analyzer.generate_visualization(str(output_dir / "simulator_call_tree.html"))
    analyzer.generate_static_visualization(str(output_dir / "simulator_call_tree.svg"))
    
    # Generate statistics
    stats = analyzer.generate_stats()
    stats_path = output_dir / "simulator_stats.json"
    with open(stats_path, 'w', encoding='utf-8') as f:
        json.dump(stats, f, indent=2)
    print(f"Statistics saved to: {stats_path}")
    
    # Compare with documentation
    if os.path.exists(args.doc_path):
        print("\nComparing with existing documentation...")
        differences = compare_with_documentation(analyzer, args.doc_path)
        
        diff_path = output_dir / "documentation_differences.json"
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
    print(f"Total function calls: {stats['total_calls']}")
    print(f"\nMost called functions:")
    for func_info in stats['most_called_functions'][:5]:
        print(f"  - {func_info['function']}: {func_info['calls']} calls")
    print(f"\nRecursive functions found: {len(stats['recursive_functions'])}")
    if stats['recursive_functions']:
        for func in stats['recursive_functions'][:5]:
            print(f"  - {func}")

if __name__ == "__main__":
    main()