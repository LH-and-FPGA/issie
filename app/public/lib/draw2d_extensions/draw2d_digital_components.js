/* 
 * Extension to draw digital logic components.
 *
 * For every figure it is necessary to define:
 * - width of the svg element
 * - hieght of the svg element
 * - shapes that will form the svg element
 * - userData (data accessible from the outside), containing at least:
 *   - componentType : string (e.g. "Not", "And", "Mux2", ...)
 * TODO: it is no longer necessary to use UserData, remove it.
 * If necessary it is possible to override the portLocator class.
 */

draw2d.shape.digital = draw2d.SVGFigure.extend({

    NAME:"draw2d.shape.digital",

    init: function(attr, setter, getter ){
        this._super($.extend({bgColor: "lightgrey"}, attr), setter, getter);
    },

    repaint: function(attributes) {
        if (this.repaintBlocked===true || this.shape === null) {
            return;
        }
        attributes= attributes || {};
        // Redirect the backgroundColor to an internal SVG node.
        // In this case only a small part of the shape are filled with the background color
        // and not the complete rectangle/bounding box.
        attributes["fill"] = "none";
        if (this.bgColor != null) {
            for (let i = 0; i < this.svgElements.length; i++) {
                if (this.svgElements[i].toFill) {
                    this.svgNodes[i].attr({fill: this.bgColor.hash()});
                }
            }
        }
        this._super(attributes);
        return this;
    },

    getSVG: function() {
        let svgFigure = `<svg xmlns="http://www.w3.org/2000/svg" width="${this.svgWidth}" height="${this.svgHeight}" version="1.1">`;
        for (let i = 0; i < this.svgElements.length; i++) {
            svgFigure += this.svgElements[i].path;
        }
        svgFigure += '</svg>'
        return svgFigure;
    },

    createDigitalPort: function(type, locator, isBusPort) {
        let port = this.createPort(type, locator);
        port.isBusPort = isBusPort;
    },
});

draw2d.shape.digital.Input = draw2d.shape.digital.extend({

    NAME:"draw2d.shape.digital.Input",

    componentType : "Input",
    svgHeight : 20,
    svgWidth : 30,
    svgElements : [
        {path: '<polygon points="0,0 20,0 30,10 20,20 0,20" stroke="black" stroke-width="1" fill="lightgray" />', toFill: true}
    ],
    numberOfBits : 1,

    init: function(attr, setter, getter ){
        this._super(
            $.extend({width:this.svgWidth, height:this.svgHeight}, attr),
            setter,
            getter
        );

        console.assert(typeof attr.numberOfBits === "number", "numberOfBits is not a number when creating an Input node");
        this.numberOfBits = attr.numberOfBits;

        this.createDigitalPort("output", new draw2d.layout.locator.OutputPortLocator(), this.numberOfBits > 1);
    },
});

draw2d.shape.digital.Output = draw2d.shape.digital.extend({

    NAME:"draw2d.shape.digital.Output",

    componentType : "Output", 
    svgHeight : 20,
    svgWidth : 30,
    svgElements : [
        {path: '<polygon points="0,10 10,0 30,0 30,20 10,20" stroke="black" stroke-width="1" fill="lightgray" />', toFill: true}
    ],
    numberOfBits : 1, // TODO: is this necessary? It can be inferred.

    init: function(attr, setter, getter ){
        this._super(
            $.extend({width:this.svgWidth, height:this.svgHeight}, attr),
            setter,
            getter
        );

        console.assert(typeof attr.numberOfBits === "number", "numberOfBits is not a number when creating an Output node");
        this.numberOfBits = attr.numberOfBits;

        this.createDigitalPort("input", new draw2d.layout.locator.InputPortLocator(), this.numberOfBits > 1);
    },
});

draw2d.shape.digital.Not = draw2d.shape.digital.extend({

    NAME:"draw2d.shape.digital.Not",

    componentType : "Not",
    svgHeight : 30,
    svgWidth : 30,
    svgElements : [
        {path: '<polygon points="0,0 20,15 0,30" stroke="black" stroke-width="1" fill="lightgray" />', toFill: true},
        {path: '<circle cx="25" cy="15" r="5" stroke="black" stroke-width="1" fill="lightgray" />', toFill: true},
    ],

    init: function(attr, setter, getter ){
        this._super(
            $.extend({width:this.svgWidth, height:this.svgHeight}, attr),
            setter,
            getter
        );

        this.createDigitalPort("input", new draw2d.layout.locator.InputPortLocator(), false);
        this.createDigitalPort("output", new draw2d.layout.locator.OutputPortLocator(), false);
    },
});

draw2d.shape.digital.And = draw2d.shape.digital.extend({

    NAME:"draw2d.shape.digital.And",

    componentType : "And",
    svgHeight : 40,
    svgWidth : 40,
    svgElements : [
        {path: '<path d="M 0 0 L 20 0 A 20 20, 0, 0, 1, 20 40 L 0 40 Z" stroke="black" stroke-width="1" fill="lightgray"/>', toFill: true}
    ],

    init: function(attr, setter, getter ){
        this._super(
            $.extend({width:this.svgWidth, height:this.svgHeight}, attr),
            setter,
            getter
        );

        this.createDigitalPort("input", new draw2d.layout.locator.InputPortLocator(), false);
        this.createDigitalPort("input", new draw2d.layout.locator.InputPortLocator(), false);
        this.createDigitalPort("output", new draw2d.layout.locator.OutputPortLocator(), false);
    },
});

draw2d.shape.digital.Or = draw2d.shape.digital.extend({

    NAME:"draw2d.shape.digital.Or",

    componentType : "Or",
    svgHeight : 40,
    svgWidth : 40,
    svgElements : [
        {path: '<path d="M 0 0 Q 15 20 0 40 Q 75 20 0 0" stroke="black" stroke-width="1" fill="lightgray"/>', toFill: true}
    ],

    init: function(attr, setter, getter ){
        this._super(
            $.extend({width:this.svgWidth, height:this.svgHeight}, attr),
            setter,
            getter
        );

        this.createDigitalPort("input", new draw2d.layout.locator.InputPortLocator(), false);
        this.createDigitalPort("input", new draw2d.layout.locator.InputPortLocator(), false);
        this.createDigitalPort("output", new draw2d.layout.locator.OutputPortLocator(), false);
    },
});

draw2d.shape.digital.Xor = draw2d.shape.digital.extend({

    NAME:"draw2d.shape.digital.Xor",

    componentType : "Xor",
    svgHeight : 40,
    svgWidth : 40,
    svgElements : [
        {path: '<path d="M -5 0 Q 10 20 -5 40" stroke="black" stroke-width="1" fill="none"/>', toFill: false},
        {path: '<path d="M 0 0 Q 15 20 0 40 Q 75 20 0 0" stroke="black" stroke-width="1" fill="lightgray"/>', toFill: true}
    ],

    init: function(attr, setter, getter ){
        this._super(
            $.extend({width:this.svgWidth, height:this.svgHeight}, attr),
            setter,
            getter
        );

        this.createDigitalPort("input", new draw2d.layout.locator.InputPortLocator(), false);
        this.createDigitalPort("input", new draw2d.layout.locator.InputPortLocator(), false);
        this.createDigitalPort("output", new draw2d.layout.locator.OutputPortLocator(), false);
    },
});

draw2d.shape.digital.Nand = draw2d.shape.digital.extend({

    NAME:"draw2d.shape.digital.Nand",

    componentType : "Nand",
    svgHeight : 40,
    svgWidth : 50,
    svgElements : [
        {path: '<path d="M 0 0 L 20 0 A 20 20, 0, 0, 1, 20 40 L 0 40 Z" stroke="black" stroke-width="1" fill="lightgray"/>', toFill: true},
        {path: '<circle cx="45" cy="20" r="5" stroke="black" stroke-width="1" fill="lightgray" />', toFill: true},
    ],

    init: function(attr, setter, getter ){
        this._super(
            $.extend({width:this.svgWidth, height:this.svgHeight}, attr),
            setter,
            getter
        );

        this.createDigitalPort("input", new draw2d.layout.locator.InputPortLocator(), false);
        this.createDigitalPort("input", new draw2d.layout.locator.InputPortLocator(), false);
        this.createDigitalPort("output", new draw2d.layout.locator.OutputPortLocator(), false);
    },
});

draw2d.shape.digital.Nor = draw2d.shape.digital.extend({

    NAME:"draw2d.shape.digital.Nor",

    componentType : "Nor",
    svgHeight : 40,
    svgWidth : 48,
    svgElements : [
        {path: '<path d="M 0 0 Q 15 20 0 40 Q 75 20 0 0" stroke="black" stroke-width="1" fill="lightgray"/>', toFill: true},
        {path: '<circle cx="43" cy="20" r="5" stroke="black" stroke-width="1" fill="lightgray" />', toFill: true},
    ],

    init: function(attr, setter, getter ){
        this._super(
            $.extend({width:this.svgWidth, height:this.svgHeight}, attr),
            setter,
            getter
        );

        this.createDigitalPort("input", new draw2d.layout.locator.InputPortLocator(), false);
        this.createDigitalPort("input", new draw2d.layout.locator.InputPortLocator(), false);
        this.createDigitalPort("output", new draw2d.layout.locator.OutputPortLocator(), false);
    },
});

draw2d.shape.digital.Xnor = draw2d.shape.digital.extend({

    NAME:"draw2d.shape.digital.Xnor",

    componentType : "Xnor",
    svgHeight : 40,
    svgWidth : 48,
    svgElements : [
        {path: '<path d="M -5 0 Q 10 20 -5 40" stroke="black" stroke-width="1" fill="none"/>', toFill: false},
        {path: '<path d="M 0 0 Q 15 20 0 40 Q 75 20 0 0" stroke="black" stroke-width="1" fill="lightgray"/>', toFill: true},
        {path: '<circle cx="43" cy="20" r="5" stroke="black" stroke-width="1" fill="lightgray" />', toFill: true},
    ],

    init: function(attr, setter, getter ){
        this._super(
            $.extend({width:this.svgWidth, height:this.svgHeight}, attr),
            setter,
            getter
        );

        this.createDigitalPort("input", new draw2d.layout.locator.InputPortLocator(), false);
        this.createDigitalPort("input", new draw2d.layout.locator.InputPortLocator(), false);
        this.createDigitalPort("output", new draw2d.layout.locator.OutputPortLocator(), false);
    },
});


draw2d.shape.digital.Mux2 = draw2d.shape.digital.extend({

    NAME:"draw2d.shape.digital.Mux2",

    componentType : "Mux2",
    svgHeight : 50,
    svgWidth : 30,
    svgElements : [
        {path: '<path d="M 0 0 L 30 13 L 30 37 L 0 50 Z" stroke="black" stroke-width="1" fill="lightgray"/>', toFill: true},
        {path: '<text x="4" y="12" fill="black" font-family="monospace">0</text>', toFill: false},
        {path: '<text x="4" y="30" fill="black" font-family="monospace">1</text>', toFill: false},
    ],

    init: function(attr, setter, getter ){
        this._super(
            $.extend({width:this.svgWidth, height:this.svgHeight}, attr),
            setter,
            getter
        );

        this.createDigitalPort("input", new draw2d.layout.locator.InputPortLocator(), false);
        this.createDigitalPort("input", new draw2d.layout.locator.InputPortLocator(), false);
        this.createDigitalPort("input", new draw2d.layout.locator.XYRelPortLocator(50, 90), false);
        this.createDigitalPort("output", new draw2d.layout.locator.OutputPortLocator(), false);
    },
});

draw2d.shape.digital.Demux2 = draw2d.shape.digital.extend({

    NAME:"draw2d.shape.digital.Demux2",

    componentType : "Demux2",
    svgHeight : 50,
    svgWidth : 30,
    svgElements : [
        {path: '<path d="M 0 13 L 30 0 L 30 50 L 0 37 Z" stroke="black" stroke-width="1" fill="lightgray"/>', toFill: true},
        {path: '<text x="19" y="12" fill="black" font-family="monospace">0</text>', toFill: false},
        {path: '<text x="19" y="30" fill="black" font-family="monospace">1</text>', toFill: false},
    ],

    init: function(attr, setter, getter ){
        this._super(
            $.extend({width:this.svgWidth, height:this.svgHeight}, attr),
            setter,
            getter
        );

        this.createDigitalPort("input", new draw2d.layout.locator.InputPortLocator(), false);
        this.createDigitalPort("input", new draw2d.layout.locator.XYRelPortLocator(50, 90), false);
        this.createDigitalPort("output", new draw2d.layout.locator.OutputPortLocator(), false);
        this.createDigitalPort("output", new draw2d.layout.locator.OutputPortLocator(), false);
    },
});

/**
 * Custom components.
 */

draw2d.shape.digital.Custom = draw2d.shape.digital.extend({

    NAME:"draw2d.shape.digital.Custom",

    componentType : "Custom",
    svgHeight : 0,
    svgWidth : 0,
    svgElements : [],

    maxStringLen: function(arr) {
        let max = 0;
        for (let i = 0; i < arr.length; i++) {
            max = Math.max(max, arr[i][0].length);
        }
        return max;
    },

    init: function(attr, setter, getter){
        this._super(attr, setter, getter);
        this.inputs = attr.inputs;   // List of tuples: string * int.
        this.outputs = attr.outputs; // List of tuples: string * int.
        this.customComponentName = attr.name; // String.

        const portSpace = 30;
        const padding = 7;
        const fontHeight = 10;
        const fontWidth = 6;
        this.svgHeight = Math.max(this.inputs.length, this.outputs.length) * portSpace;
        this.svgWidth = Math.max(
            50,
            30 + (this.maxStringLen(this.inputs) + this.maxStringLen(this.outputs)) * fontWidth
        )

        this.svgElements = [{
            path: `<rect height="${this.svgHeight}" width="${this.svgWidth}" stroke="black" stroke-width="1" fill="lightgray"/>`,
            toFill: true
        }]

        for (let i = 0; i < this.inputs.length; i++) {
            const inputLabel = this.inputs[i][0];
            const inputWidth = this.inputs[i][1];
            this.createDigitalPort("input", new draw2d.layout.locator.InputPortLocator(), inputWidth > 1);
            const x = padding;
            const y = this.svgHeight / (this.inputs.length + 1) * (i + 1) - fontHeight / 2;
            this.svgElements.push({
                path: `<text x="${x}" y="${y}" fill="black" font-family="monospace">${inputLabel}</text>`,
                toFill: false
            });
        }
        for (let i = 0; i < this.outputs.length; i++) {
            const outputLabel = this.outputs[i][0];
            const outputWidth = this.outputs[i][1];
            this.createDigitalPort("output", new draw2d.layout.locator.OutputPortLocator(), outputWidth > 1);
            const x = this.svgWidth - padding - fontWidth * outputLabel.length;
            const y = this.svgHeight / (this.outputs.length + 1) * (i + 1) - fontHeight / 2;
            this.svgElements.push({
                path: `<text x="${x}" y="${y}" fill="black" font-family="monospace">${outputLabel}</text>`,
                toFill: false
            });
        }
    },
});

/**
 * Buses.
 */

draw2d.shape.digital.MergeWires = draw2d.shape.digital.extend({

    NAME:"draw2d.shape.digital.MergeWires",

    componentType : "MergeWires",
    svgHeight : 20,
    svgWidth : 40,
    svgElements : [],  

    init: function(attr, setter, getter ){
        this._super(
            $.extend({width:this.svgWidth, height:this.svgHeight}, attr),
            setter,
            getter
        );

        this.svgElements = [
            // Horizontal line in center.
            {path: `<line x1="${this.svgWidth/2}" y1="${this.svgHeight/2}" x2="${this.svgWidth}" y2="${this.svgHeight/2}" stroke="purple" stroke-width="3" />`, toFill: false},
            // Vertical to top.
            {path: `<line x1="${this.svgWidth/2}" y1="${this.svgHeight/2}" x2="${this.svgWidth/2}" y2="0" stroke="black" stroke-width="1" />`, toFill: false},
            // Top horizontal.
            {path: `<line x1="0" y1="0" x2="${this.svgWidth/2}" y2="0" stroke="black" stroke-width="1" />`, toFill: false},
            // Vertical to bottom.
            {path: `<line x1="${this.svgWidth/2}" y1="${this.svgHeight/2}" x2="${this.svgWidth/2}" y2="${this.svgHeight}" stroke="black" stroke-width="1" />`, toFill: false},
            // Bottom horizontal.
            {path: `<line x1="0" y1="${this.svgHeight}" x2="${this.svgWidth/2}" y2="${this.svgHeight}" stroke="black" stroke-width="1" />`, toFill: false},
        ];

        this.createDigitalPort("input", new draw2d.layout.locator.XYRelPortLocator(0, 0), false);
        this.createDigitalPort("input", new draw2d.layout.locator.XYRelPortLocator(0, 100), false);
        this.createDigitalPort("output", new draw2d.layout.locator.XYRelPortLocator(100, 50), true);
    },
});

draw2d.shape.digital.SplitWire = draw2d.shape.digital.extend({

    NAME:"draw2d.shape.digital.SplitWire",

    componentType : "SplitWire",
    svgHeight : 20,
    svgWidth : 40,
    svgElements : [],
    numberOfBitsInTopWire : 1,

    init: function(attr, setter, getter ){
        this._super(
            $.extend({width:this.svgWidth, height:this.svgHeight}, attr),
            setter,
            getter
        );

        console.assert(typeof attr.numberOfBitsInTopWire === "number", "numberOfBitsInTopWire is not a number when creating an SplitWire node");
        this.numberOfBitsInTopWire = attr.numberOfBitsInTopWire;

        this.svgElements = [
            // Horizontal line in center.
            {path: `<line x1="0" y1="${this.svgHeight/2}" x2="20" y2="${this.svgHeight/2}" stroke="purple" stroke-width="3" />`, toFill: false },
            // Vertical to top.
            {path: `<line x1="${this.svgWidth/2}" y1="${this.svgHeight/2}" x2="${this.svgWidth/2}" y2="0" stroke="black" stroke-width="1" />`, toFill: false },
            // Top horizontal.
            {path: `<line x1="${this.svgWidth/2}" y1="0" x2="${this.svgWidth}" y2="0" stroke="black" stroke-width="1" />`, toFill: false },
            // Vertical to bottom.
            {path: `<line x1="${this.svgWidth/2}" y1="${this.svgHeight/2}" x2="${this.svgWidth/2}" y2="${this.svgHeight}" stroke="black" stroke-width="1" />`, toFill: false },
            // Bottom horizontal.
            {path: `<line x1="${this.svgWidth/2}" y1="${this.svgHeight}" x2="${this.svgWidth}" y2="${this.svgHeight}" stroke="black" stroke-width="1" />`, toFill: false },
        ];

        this.createDigitalPort("input", new draw2d.layout.locator.XYRelPortLocator(0, 50), true);
        this.createDigitalPort("output", new draw2d.layout.locator.XYRelPortLocator(100, 0), false);
        this.createDigitalPort("output", new draw2d.layout.locator.XYRelPortLocator(100, 100), false);
    },
});
