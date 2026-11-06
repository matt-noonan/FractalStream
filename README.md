# FractalStream
An updated version of FractalStream (http://www.math.cornell.edu/~noonan/fstream.html),
a tool to assist research in real and complex dynamical systems.

# Installation

## MacOS

There are two ways to get FractalStream on a Mac:

### via GitHub releases
From the [Releases](https://github.com/matt-noonan/FractalStream/releases) page,
go to the most recent release, click `Assets`, and choose the MacOS installer
for your system (`FractalStream-<version>-macos-arm64.zip` for modern Macs,
or `FractalStream-<version>-macos-x86_64.zip` for older Intel-based Macs).

### via `homebrew`
Install [Homebrew](https://brew.sh), open `Terminal.app`, and run these commands:
``` shell
brew tap matt-noonan/fractalstream
brew trust matt-noonan/fractalstream
brew install --cask fractalstream
```

## Linux

The simplest way to get FractalStream on Linux is to use the AppImage.
From the [Releases](https://github.com/matt-noonan/FractalStream/releases) page,
go to the most recent release, click `Assets`, and choose
`FractalStream-<version>-linux-x86_64.AppImage`.

## Windows

The simplest way to get FractalStream on Linux is to use the AppImage.
From the [Releases](https://github.com/matt-noonan/FractalStream/releases) page,
go to the most recent release, click `Assets`, and choose
`FractalStream-<version>-windows-x86_64-setup.exe`.

# Getting started

## Parts of a FractalStream script

A FractalStream script defines one or more *viewers* (optionally equipped with *tools*),
and optionally a number of *configuration variables*.
![FractalStream project windows generated from a script](https://raw.githubusercontent.com/matt-noonan/FractalStream/refs/heads/assets/assets/FractalStream%20script%20components.jpg)
