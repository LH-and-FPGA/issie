# Issie - an Interactive Schematic Simulator with Integrated Editor

Issie (Interactive Schematic Simulator with Integrated Editor) is an application for digital circuit design and simulation. It is targeted at students and hobbyists that want to get a grasp of Digital Electronics concepts in a simple and fun way. Issie is designed to be beginner-friendly and guide the users toward their goals via clear error messages and visual clues. Issie is developed and actively used in teaching at Imperial College London.

* If you are just interested in using the application, jump to the [Getting Started](#getting-started) section. 
* If you want user documentation and news go to the [web pages](https://tomcl.github.io/issie/).
* If you are interested in a more detailed description of Issie please check out the [Wiki](https://github.com/tomcl/issie/wiki).

For more technical info about the project, read on. This documentation is partly based on the excellent [VisUAL2](https://github.com/ImperialCollegeLondon/Visual2) documentation, given the similarity in the technology stack used.

## Introduction

For the Issie website go [here](https://tomcl.github.io/issie/).

The application is mostly written in F#, which gets transpiled to JavaScript via the [Fable](https://fable.io/) compiler. [Electron](https://www.electronjs.org/) is then used to convert the developed web-app to a cross-platform application. [Electron](electronjs.org) provides access to platform-level APIs (such as access to the file system) which would not be available to vanilla browser web-apps.

[Webpack 5](https://webpack.js.org/) is the module bundler responsible for the JavaScript concatenation and automated building process: the electron-webpack build 
is automated using the pre-existing scripts under the [scripts](scripts/) directory.

The drawing capabilities are provided (now) by a custom schematic editor library implemented in F# and specialised for digital components.

The choice of F# as main programming language for the app has been dictated by a few factors:

* The success of the [VisUAL2](https://github.com/ImperialCollegeLondon/Visual2), which uses a similar technology stack;
* Strongly typed functional code tends to be easy to maintain and test, as the type-checker massively helps you;
* Imperial College EEE/EIE students learn such language in the 3rd year High-Level-Programming course, hence can maintain the app in the future;
* F# can be used with the powerful [Elmish](https://elmish.github.io/elmish/) framework to develop User Interfaces in a [Functional Reactive Programming](https://en.wikipedia.org/wiki/Functional_reactive_programming) fashion.


## Getting Started

If you just want to run the app go to the [releases page](https://github.com/tomcl/issie/releases) and
download and run the latest prebuilt binary for your platform (Windows or Macos). Issie will require in total about 200M of disk space.

* Windows: unzip \*.zip anywhere and double-click the top-level `Issie.exe` application in the unzipped files.
    * If you get a security warning saying something like: *Microsoft Defender SmartScreen prevented an unrecognized app from starting. Running this app might put your PC at risk.
More info* then:
        * Click **More Info**
        * Then click **Run Anyway**
* Macos: Double click the dmg file  and run the application inside the folder, or drag and drop this to install.
    * If Macos asks you to do this, you will need to change your security settings to allow apps not downloaded from app store
    * *Apple* -> *System settings* -> *Privacy & Security* -> (find at bottom of options by scrolling) *Allow Applications From* ->  *App Store and Known Developers*

      

Issie installs and runs without making system changes - all of its code is inside the directory you download. 
You can delete this and replace it by a later version of Issie. Each design sheet is stored in a similarly named file under the project directory. 
The subdirectory `backup` there contains a large numbers of backup snapshots for design recovery. 
These are not needed for Issie operation so you can delete them - or even the whole `backup` directory, if you wish.

Issie binaries will not run (in some cases) from a networked file location (found on many cluster machines). 
If you have this problem navigate to the top-level directory containing the Issie binaries in a command window 
and type `issie.exe --no-sandbox`. See https://github.com/tomcl/issie/issues/125 for details.

Once you open up Issie and are ready to go, feel free to open one of the Demo Projects from the start-up window. These are there to show you what a complete Issie project looks like and enable you to have fun with it without having to design and build it from scratch. Every time you reopen a demo project it will be reset to its initial state.

## Getting Started as Developer

If you want to get started as a developer, follow these steps.

### Development Install Prerequisites (common to windows, macos, linux)

Download and install (if you already have these tools installed just check the version constraints).


* [.Net 8 SDK](https://dotnet.microsoft.com/download/dotnet/8.0).  
* [Node.js v22](https://nodejs.org/en/download/prebuilt-installer/current).
    * You do not need to install chocolatey (at the prompt for this) however you can if you want
    * Node.js includes the `npm` package manager, so this does not need to be installed separately.
         * However as of Dec 2024 v22 node does not include the necessary latest v11 npm. After installing node you must update npm as follows
              * npm install -g npm@latest
              * After upgrade npm --version should return 11.x.x
    * If you are using a different version of Node for development on other projects, global install 
    (the default) may interfere with this. You will need to do a more complex local node install.
* (recommended) Visual Studio 2022 which includes F# 8.0, Install with:
  * Workload: .Net Desktop development
  * Ticked: F# language support
* (recommended) install [hyper.js](https://hyper.is/) or [Windows Terminal](https://github.com/microsoft/terminal) for Windows. On windows change terminal settings if needed so terminal runs cmd window not powershell.

### Issie Development

1. Download & unzip the [Issie repo](https://github.com/tomcl/ISSIE), or clone it locally, or fork it on github and then clone it locally.
   - If downloading as zip you must on Windows unblock the zip file's MOTW mark. In file explorer, right-clik on the zip file and select properties. Then click `unblock`.
   - Afte unblocking extract all files from the zip file as normal.

3. Check you have the .NET and Node prerequisites above if you want to do more than make binaries, also: VS 2022 (or latest VS Code + ionide, or Rider) installed.
   * In a terminal window: `node -v` shows Node version. `dotnet --version` shows Dotnet version.

4. Navigate from the repo master branch root directory (which contains this README), in a command-line interpreter, or start one from directory context menu.

5. Run `build.cmd` under Windows or `build.sh` under linux or macos (`chmod 755 build.sh` will give the script execute permission). This will download and install all dependencies then launch the application in dev mode with HMR.
     
  * HMR: the application will automatically recompile and update while running if you save updated source files
  * To initialise and reload: `File -> reload page`
  * To exit: after you exit the application the auto-compile script will terminate after about 15s
  * To recompile the whole application again run `npm run dev`. Run `npm run debug` for the debug mode (this is going to be a lot slower than dev).
  * To generate distributable binaries for dev host system `npm run dist`.
  * If you have changed `packet.json` and therefore need to remake the lock file `paket-lock.json` use `npm install`.
  * On windows `build killzombies` will terminate orphan node and dotnet processes which occasionally happen using this build chain after unusual terminations (maybe no longer needed?)

6. **To make binaries only**. Cancel dev mode (two ctrl-C in command window) if it is running. Run `npm run dist` in command window to generate binaries under `.\dist` directory. For macos you will need to install python 3 to compile native binaries - you will be auto-prompted to do this but will then need to run `npm run dist` again.

NB - in parallel with the above compilation, Issie code will always compile without errors (but not run) under dotnet, for example by building it from Visual Studio. Compilation should be identical but when unsure why there is an error it is **very helpful** to build the current code under .Net with VS or VSC and get easier to find error messages. Similarly, VS or VSC can be used with confidence to refactor code, testing with compilation. Building under VS or VSC cannot work because the code depends on Electron and Node APIs to work.

#### Node management details

* `package-lock.json` contains exact package versions and is downloaded from the repo. Normally you don't need to change this. The standard build above will run `npm ci` which checks and audits packages but does not change the lock file.
* If you need to add upgrade a package (in `package.json1`) use `npm install` to recreate a lock file, which can be pushed to the repo.
* Single packages can conveniently be changed or added using `npm upgrade name` or `npm [-D] install name` instead of editing `package.json`.  
* If a package audits with a problem use `npm ls name` to find which of the required packages use it (usually upgrading or replacing them will remove the problem).

#### Development on Macos

A clean build will work equally well on macos, however things are more likely to go wrong if you have previously installed conflicting packages:

* Legacy versions of `dotnet` - can if needed be removed [as here](https://stackoverflow.com/questions/44089518/how-can-i-uninstall-dotnet-core-from-macos-sierra):

  ```bash
  curl -O https://raw.githubusercontent.com/dotnet/sdk/main/scripts/obtain/uninstall/dotnet-uninstall-pkgs.sh
  chmod u+x dotnet-uninstall-pkgs.sh
  sudo ./dotnet-uninstall-pkgs.sh
  ```

* Root permissions in dev files. For dev to work smoothly you need every configuration file to be installed under your own username, so you have r/w access. This will break if you ever find yourself using `sudo` to root install software, or if you have done this some time in the past. In that case you can temporarily get round issues by using `sudo` to run the development (or the generated app) with admin privileges. This is the wrong thing to do. Instead you should use
  * ``chown -R `whoami` dir``
for each directory that might have the files with bad permissions. Typically your dev directory `.` and `/usr/local`.
* Uninstalling and reinstalling latest dotnet is helpful if dotnet has been installed wrong.
* For Apple silicon Mac users, you should use the Arm64 version of .NET in order to get the best results. You can get it from the official Microsoft Website, using their installer. 


### Under the hood for developers

Although the dev chain is complex, it is now very smooth and identical for all platforms. Each of these steps can be performed as needed:

1. You need `Dotnet SDK` and `Node` installed. Dotnet SDK gives you F#.
2. `dotnet tool restore` gets you the dev tools: `Fable` compiler, `Fake` build automation, `paket` dotnet package manager. (Node package management is via `npm` which comes with Node).
3. `dotnet paket install` installs all of the dotnet-side packages needed
4. `npm ci` downloads and audits correct versions of all of the npm packages. `npm install` will redo the versions if these have changed and generate an updated lock file.
5. `npm run dev`, `npm run dist`, `npm run debug`: scripts defined in `package.json` which control developmment (with HMR) or production compilation with Fable, and packing using Webpack 5.
6. The `build.cmd` and `build.sh` scripts package the above steps adding some not usually necessary directory cleaning - you can run them individually in order if you have problems.

* To update the tool versions (not normally needed) edit `dotnet-tools.json`.
* To change the dotnet packages used (advanced) change `paket.dependencies` at top level **and** `paket.references` in the directory of the relevant `.fsproj` file. Currently dotnet packages are not pinned to versions so latest compatible versions are always used. This is probably wrong but seems to work well.
* To interface to a new Node package from F# see the excellent [Fable documentation](https://fable.io/docs/communicate/js-from-fable.html). The **best** way to do this is to write an F# interface file which provides
static typing (like a typescript definition file). In fact there is a wonderful automatic converter [ts2fable](https://github.com/fable-compiler/ts2fable) which generates F# interfaces from typescript `.d` files. This works well, but manual adjustment is needed for anything complex. See [the Electron API interface](https://github.com/tomcl/issie/blob/master/src/Renderer/Common/ElectronAPI.fs) in Issie which was generated in this way from a published electron API `.d` files - in that case the manual adjustment was quite unpleasant because Electron API is very complex.
* To understand Elmish and MVU read the excellent [Elmish book](https://zaid-ajaj.github.io/the-elmish-book/#/)
* For more documentation on Issie in addition to XML code comments see the [Issie Wiki](https://github.com/tomcl/issie/wiki)


## Project Structure

Electron bundles Chromium (View) and node.js (Engine), therefore as in every node.js project, the `package.json` file specifies the (Node) module dependencies.

* dependencies: node libraries that the executable code (and development code) needs
* dev-dependencies: node libraries only needed by development tools

Additionally, the section `"scripts"`:
```
"scripts": {
    "clean-dev-mac": "sudo killall -9 node && sudo killall -9 dotnet && sudo killall -9 issie",
    "clean-dev-win": "taskkill /f /im node.exe && taskkill /f /im dotnet.exe && taskkill /f /im issie.exe",
    "compile": "dotnet fable src/Main -s && dotnet fable src/Renderer -s --define PRODUCTION",
    "debug": "dotnet fable watch src/Main -s --run npm run debugrenderer",
    "debugrenderer": "dotnet fable watch src/Renderer -s --define ASSERTS --run npm run start",
    "dev": "dotnet fable watch src/Main -s --run npm run devrenderer",
    "devrenderer": "dotnet fable watch src/Renderer -s --run npm run start",
    "start": "cross-env NODE_ENV=development node scripts/start.js",
    "build": "cross-env NODE_ENV=production node scripts/build.js",
    "pack": "npm run compile && npm run build && electron-builder --dir",
    "dist": "npm run compile && npm run build && electron-builder",
    "buildonly": "electron-builder",
    "compile-sass": "cd src/renderer/scss && node-sass main.scss main.css",
    "testcompiler": "cd src/Renderer/VerilogComponent/test && dotnet fable --noCache && node testParser.fs.js"
  }
```
Defines the in-project shortcut commands as a set of `<key> : <value` lines, so that when we use `npm run <stript_key>` it is equivalent to calling `<script_value>`. 
For example, in the root of the project, running in the terminal `npm run dev` is equivalent to the command line:

```
dotnet fable watch src/Main -s --run npm run devrenderer
```

This runs fable 4 to transpile the main process, then (`--run` is an option of fable to run another command) runs script `devrenderer` to transpile to javascript and watch the F# files in the renderer process. After the renderer transpilation is finished 
[start.js script](scripts/start.js) will be run. This invokes `webpack` to pack and lauch the javascript code, under electron, and also watches for changes in the javascript code, and *hot loads* these on the running application

As result of this, at any time saving an edited F# renderer project file causes (nearly) immediate:

* fable transpile to from F# to javascript file (dependent F# files may also be transpiled)
* webpack hot load of any changed javascript files to the running electron application

The build system depends on a `Fake` file `build.fsx`. Fake is a DSL written in F# that is specialised to automate build tasks. Build.fsx has targets representing build tasks, and normally these are run via `build.cmd` or `build.sh`, instead of using `dotnet fake` directly:

* `build <target>` ==> `dotnet fake build -t <target>`

## Code Overview

The source code consists of two distinct sections transpiled separately to Javascript to make a complete Electron application.

* The electron main process runs the Electron parent process under the desktop native OS, it starts the app process and provides desktop access services to it.
* The electron client (app) process runs under Chromium in a simulated browser environment (isolated from the native OS).

Electron thus allows code written for a browser (HTML + CSS + JavaScript) to be run as a desktop app with the additional capability of desktop filesystem access via communication between the two processes.

Both processes run Javascript under Node.

The `src/Main/Main.fs` source configures electron start-up and is boilerplate. It is transpiled to the root project directory so it can be automatically picked up by Electron.

The remaining app code (in )


The code that turns the F# project source into `renderer.js` is the FABLE compiler followed by the Node Webpack bundler that combines multiple Javascript files into a single `renderer.js`.

The compile process is controlled by the `.fsproj` files (defining the F# source) and `webpack.additions.main.js`, `webpack.additions.renderer.js`
which define how Webpack combines F# outputs for both electron main and electron app processes and where the executable code is put. 
This is boilerplate which you do not need to change; normally the F# project files are all that needs to be modified.


## Documentation and Generation
There is a script in the root of the repository, `build_docs.sh`, which will build the documentation for the project using [fsdocs](https://fsprojects.github.io/FSharp.Formatting/). The project must be compile-ready before generating the documentation.

Markdown files under `/docs` are turned into static pages on the documentation site. Any XML comments in the code are turned into documentation comments for every function in the codebase.

To add an update, go to the `/docs/updates` folder and create a new markdown file with the following headers:

```markdown
---
layout: post
title:  [title here]
date:   [ ISO 8601 UTC datetime, etc 2021-07-04 15:52:01 +0100]
category: Updates
index: [index that decides the order of the update. later updates have greater indexes]
---
# your markdown content below
```

See other docs in the `/docs/updates` folder for examples.

All XML comments  (starting with `///`)  under any module and function declarations are turned into documentation under the API Reference section of the documentation website. 

> **Please follow XML rules when creating documentation comments in the code, i.e. no usage of triangular brackets < and > other than for tags. Please do not use double quotes as well!**

`build_docs.sh` also calls `dotnet fsdocs watch` to start a local server hosting the documentation at http://localhost:8901/. The generated documentation for the code is under the "API REFERENCE" section. 

If you've built the docs and want to access the server again, you can run `dotnet fsdocs watch` in the terminal.

> Side note: A script, rather than the usual `dotnet fsdocs build` is used due to an undocumented bug where the compiler creates invalid XML code for functions with anonymous records, assigning attributes with "<>" in their names. This causes the generation to fail. Using `<exclude/>` does not fix the issue, so a workaround is to call a script that uses regex to remove these invalid attributes from the XML documentation before building the documentation. <br> See a similar issue on GitHub that throws a similar error [here.](https://github.com/fsprojects/FSharp.Formatting/issues/707) 

## File Structure

### `src` folder

|   Subfolder or file   |                                             Description                                 |
|:----------------------|:----------------------------------------------------------------------------------------|
| `Main/main.fs` | Code for the main electron process that sets everything up - not normally changed |
| `Renderer/Common/*`       | Provides some common types and utilities, as well as interfaces to libraries APIs and custom libraries |
| `Renderer/Interface/*` | Contains low-level interface functions, and all the low-level file management              |
| `Renderer/DrawBlock/*` | Contains all the SVG-based schematic editor code in F#|
| `Renderer/Simulator/*` | Contains the logic to analyse and simulate a schematic sheet                               |             
| `Renderer/UI/*`     | Contains the UI logic|
| `./renderer.fs`     | Top-level file that drives the renderer code: contains Elmish MVU loop and Electron menu code |

### `Tests` folder

Currently tests are very old, and will not work. They are based on F# Expecto testing library and in principle the widthinferrer and simulator code (which runs under dotnet) could be tested here.


### `Static` folder

Contains static files used in the application.

### `Docs` folder

Contains source information that controls the project documentation web site [https://tomcl.github.io/issie/](https://tomcl.github.io/issie/).

## Project versus File in the Issie application

Issie allows the users to create projects and files within those projects. A Issie project is simply a folder named `<project-name>` that contains an empty file named `<project_name>.dprj` (dprj stands for diagram project). The project folder any non-zero number of design files, each named `<component_name>.dgm` (dgm stands for diagram). each design file represents one design sheet of a hierarchical hardware design, sheets can contain, as components, other sheets.

When opening a project, Issie will initially search the given repository for `.dgm` files, parse and load their content, and allow the user to open them in Issie or use them as components in other designs.


## Reinstalling Compiler and Libraries

To reinstall the build environment (without changing project code) rerun `build.cmd` (Windows) or `build.sh` (Linux and MacOS). 

## Creating binaries

`npm run dist` will generate the correct binaries for your system under `/dist`. 



