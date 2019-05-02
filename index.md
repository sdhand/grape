# Introduction

Grape is a visual editor for the [GP 2 programming language](https://timothyatkinson.github.io/). It provides both a [host graph editor](https://sdhand.github.io/grape/graph), and a [rule editor](https://sdhand.github.io/grape/rule).

## Usage

The basic graph editing interface described in the table below is the same across both editors. Selected elements can be edited using the toolbar at the top. Note that changing the label or ID of an element requires confirmation by means of the button next to the input, or the enter key. This prevents the entering of invalid attributes.

| Operation             | Action                                     |
|:--------------------- | ------------------------------------------:|
| Create Node           | Double click background                    |
| Move Node             | Left click and drag node                   |
| Create Edge           | Right click and drag from source to target |
| Select Item           | Left click item                            |
| Delete selected item  | Press delete on keyboard                   |
| Zoom In/Out           | Mouse wheel up/down                        |
| Pan                   | Left click and drag background             |


Adding an element to the left hand side of a rule will automatically add it to the right hand side. This way, any modifications made by the rule are explicitly stated by modifying the right hand side of the rule. For example, to create a rule that deletes a node, first create the node in the left hand side, then delete it from the right hand side.

Graphs and rules can both be saved in the DOT and GP 2 formats with the buttons in the toolbar. The default format is DOT, and the dropdown can be used to select GP 2 instead. The DOT representation for rules differentiates between created and deleted elements through the use of shapes.

The rule declaration and condition can be set in the text boxes above and below the rule, respectively.

## Installation

If you wish to host the editor yourself then just clone the repository and run `make`. This requires that you have elm installed. A set of files will be produced in the `build` directory. Copy these to somewhere where they can be served with a webserver.
