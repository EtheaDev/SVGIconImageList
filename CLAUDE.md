# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

SVGIconImageList is a comprehensive Delphi component library for rendering SVG images in both VCL and FMX applications. The library provides four main components to simplify the use of SVG icons with features like resizing, fixed colors, grayscale conversion, and opacity control.

**Supported Platforms:**
- VCL: Windows (Delphi XE3 to Delphi 13)
- FMX: Cross-platform including Windows, macOS, iOS, Android, Linux

## Build System

### Package Structure

Packages are organized by Delphi version in `Packages/` directory:
- `D10_4/` - Delphi 10.4 Sydney
- `D11/` - Delphi 11 Alexandria
- `D12/` - Delphi 12 Athens
- `D13/` - Delphi 13 Florence
- `DXE3/`, `DXE6/`, `DXE7/`, `DXE8/` - Legacy Delphi versions
- And various D10.x versions

### Main Packages

For each Delphi version, there are typically 4-6 packages:

**VCL Packages:**
1. `SVGIconImageList.dpk` - Runtime VCL components package
2. `dclSVGIconImageList.dpk` - Design-time VCL components package
3. `SVGIconImageListRestClient.dpk` - REST API client for downloading icons from iconify.design
4. `SVGImage32Package.dpk` - Image32 SVG rendering engine package
5. `SVGMagicPackage.dpk` - SVGMagic SVG rendering engine package (supports animated SVG)
6. `dclSVGMagicPackage.dpk` - Design-time SVGMagic package

**FMX Packages:**
1. `SVGIconImageListFMX.dpk` - Runtime FMX components package
2. `dclSVGIconImageListFMX.dpk` - Design-time FMX components package

### Building Packages

The project includes an automatic installer (`Setup/`) that detects Delphi versions, builds, and installs packages. For manual building:

1. Open the appropriate package for your Delphi version in `Packages/[version]/`
2. Build in this order:
   - `SVGImage32Package.dpk` (if using Image32 engine)
   - `SVGMagicPackage.dpk` (if using SVGMagic engine)
   - `SVGIconImageListRestClient.dpk`
   - `SVGIconImageList.dpk` or `SVGIconImageListFMX.dpk`
   - Design-time packages last: `dclSVGIconImageList.dpk`, `dclSVGMagicPackage.dpk`, or `dclSVGIconImageListFMX.dpk`

## SVG Rendering Engines

The library supports four different SVG rendering engines, configured via `Source/SVGIconImageList.inc`:

### For VCL Applications

**Image32 (Default):** `{$DEFINE Image32_SVGEngine}`
- Native Delphi implementation by Angus Johnson
- Included in `Image32/` directory
- Supports blur effects
- Good performance across different icon types

**SVGMagic:** `{$DEFINE SVGMagic_SVGEngine}`
- Advanced SVG implementation by Ursa Minor Ltd.
- Included in `SVGMagic/` directory
- **Unique feature: Supports animated SVG files**
- Comprehensive SVG 1.1 support with advanced rendering capabilities
- Excellent rendering quality for complex SVG files

**Skia4Delphi:** `{$DEFINE Skia_SVGEngine}`
- Uses Google's Skia Graphics Library wrapper
- Requires separate Skia4Delphi installation
- Supports blur effects
- Best performance with simple icons

**Direct2D (Windows only):** `{$DEFINE PreferNativeSvgSupport}`
- Native Windows implementation wrapper
- Requires Windows 10 Creators Update or later
- Does NOT support blur effects or text elements
- Can enable GPU acceleration with `{$DEFINE GPUSupport}`

### For FMX Applications

**Image32 (Default):** `{$DEFINE FMX_Image32_SVGEngine}`
- Cross-platform native Delphi implementation
- Supports all FMX platforms including mobile

**Skia4Delphi:** `{$DEFINE FMX_Skia_SVGEngine}`
- Cross-platform Skia Graphics Library wrapper

**Important:** Only ONE engine can be active at a time per platform (VCL/FMX). The `.inc` file enforces this with compiler directives.

## Core Architecture

### Component Hierarchy

```
TSVGIconImageListBase (base class)
├── TSVGIconImageList - VCL ImageList with embedded SVG collection
├── TSVGIconVirtualImageList - VCL Virtual ImageList (from D10.3+, inherits from TVirtualImageList)
└── TSVGIconImageListFMX - FMX ImageList

TSVGIconImageCollection - Centralized collection of SVG images (VCL only, from D10.4+ inherits from TCustomImageCollection)

TSVGIconImage - VCL/FMX Image component for displaying single SVG icons
```

### Key Source Files

**Core Components (VCL):**
- `SVGIconImageList.pas` - Main ImageList component
- `SVGIconImageCollection.pas` - Image collection component
- `SVGIconVirtualImageList.pas` - Virtual ImageList component
- `SVGIconImage.pas` - Single image display component
- `SVGIconImageListBase.pas` - Base class with common functionality

**Core Components (FMX):**
- `FMX.SVGIconImageList.pas` - FMX ImageList component
- `FMX.SVGIconImage.pas` - FMX Image component
- `FMX.SVGIconsUtils.pas` - FMX utilities

**Infrastructure:**
- `SVGInterfaces.pas` - Core interfaces (ISVG, ISVGFactory) for rendering abstraction
- `SVGIconItems.pas` - SVG icon item and collection classes
- `SVGIconUtils.pas` - Utility functions
- `SVGMessaging.pas` - DPI change messaging support

**Engine Implementations:**
- `Image32SVGFactory.pas` - Image32 engine factory
- `SVGMagicFactory.pas` - SVGMagic engine factory
- `SkiaSVGFactory.pas` - Skia4Delphi engine factory
- `D2DSVGFactory.pas` - Direct2D engine factory
- `FMX.Image32SVG.pas` - FMX Image32 implementation
- `FMX.ImageSkiaSVG.pas` - FMX Skia implementation

**Additional Features:**
- `Browser.IconifyApi.pas` - REST API client for iconify.design integration
- `dlgExportPNG.pas` - PNG export dialog
- `Winapi.D2DMissing.pas` - Missing Direct2D declarations

### Factory Pattern

The library uses a factory pattern to abstract SVG rendering:

1. `GlobalSVGFactory` provides the active ISVGFactory instance
2. The factory is set at unit initialization based on `SVGIconImageList.inc` defines
3. Components create ISVG instances through `GlobalSVGFactory.NewSvg`
4. This allows switching engines without changing component code

### Properties Available on Components

**Common Properties:**
- `FixedColor` - Apply a fixed color to all icons
- `GrayScale` - Render icons in grayscale
- `Opacity` - Set transparency level
- `ApplyFixedColorToRootOnly` (or `ApplyToRootOnly`) - Apply fixed color only to root SVG elements
- `DisabledGrayScale` - Grayscale for disabled state
- `DisabledOpacity` - Opacity for disabled state

**TSVGIconImageCollection Properties (D10.3+):**
- Inherited from `TVirtualImageList`, properties affect all linked VirtualImageLists
- Use `TSVGIconVirtualImageList` properties to override per VirtualImageList
- `PreserveItems` - Important for maintaining ImageIndex stability when adding/removing icons

## Demo Applications

Located in `Demo/` directory:

- **SVGIconImageListDemo** - Main demo showing all component features
- **SVGIconVirtualImageListDemo** - Demonstrates multiple VirtualImageLists sharing one Collection
- **SVGExplorer** - Utility to browse and preview SVG icon sets
- **SVGViewer** - Compare rendering quality of different engines
- **SVGMagic** - Comprehensive demo showcasing SVGMagic library components including:
  - TWSVGImage, TWSVGImageButton, TWSVGImageList components
  - Styled checkboxes and radio buttons with SVG graphics
  - SVG browser with animation support
  - Real-world examples (banking and payment forms)
  - Demonstrates animated SVG rendering capabilities
- **Benchmark** - Performance testing application
- **CategoryButtonTest** - TControlList component integration (D10.4.2+)

Each demo has project files for multiple Delphi versions (DXE3, DXE6, DXE8, D10, D10_1, D11, D12, D13).

**All demos have been updated to support and test the new SVGMagic engine**, including its unique capability to render animated SVG files.

## Version-Specific Conditional Compilation

The project uses extensive version-based defines in `SVGIconImageList.inc`:
- `{$IFDEF DXE3+}` through `{$IFDEF D13+}` for version-specific features
- `{$IFDEF HiDPISupport}` - Available from D10.3+, enables DPI change messaging
- `{$IFDEF UseRESTClientSearch}` - Available from DXE8+, enables web icon download

## Important Implementation Notes

### High-DPI Support

From Delphi 10.3+, the components support automatic DPI scaling:
- Components respond to `System.Messaging.TStyleChangedMessage` for DPI changes
- Use `DPIChanged` method to manually trigger DPI updates
- Icons are automatically re-rendered at the correct size for current DPI

### VirtualImageList Behavior (D10.3+)

`TSVGIconVirtualImageList` inherits from `TVirtualImageList`:
- May create only a subset of images from the collection
- Has its own `FixedColor`, `GrayScale`, `ApplyToRootOnly`, and `Opacity` properties
- Multiple VirtualImageLists can share one `TSVGIconImageCollection` with different properties
- Changes to Collection-level properties affect ALL linked VirtualImageLists

**Recommended Pattern (D10.3+):** Use `TSVGIconImageCollection` + `TSVGIconVirtualImageList` instead of standalone `TSVGIconImageList`.

### Component Editor Features

Both VCL and FMX component editors support:
- Adding icons from files
- Adding icons from WEB via iconify.design API (requires `SVGIconImageListRestClient` package)
- Editing SVG text directly with syntax validation
- Organizing icons by categories
- Exporting to PNG (single or batch)
- Preview rendering
- IDE theme support (light/dark)

## Common Development Scenarios

### Switching SVG Engines

1. Edit `Source/SVGIconImageList.inc`
2. For VCL: Enable ONE of: `Image32_SVGEngine`, `SVGMagic_SVGEngine`, `Skia_SVGEngine`, or `PreferNativeSvgSupport`
3. For FMX: Enable ONE of: `FMX_Image32_SVGEngine` or `FMX_Skia_SVGEngine`
4. Rebuild all packages in order (runtime before design-time)
5. Reinstall design-time packages in IDE

**Note:** SVGMagic engine is the only engine that supports animated SVG files, making it the ideal choice for applications requiring SVG animations.

### Adding Icons Programmatically

```delphi
// Using Add method
SVGIconImageList.Add(SVGFactory.NewSvg, 'IconName', 'Category',
  False, // GrayScale
  clNone, // FixedColor
  clBtnFace // AntiAliasColor
);

// Loading from file
var SVG: ISVG;
SVG := GlobalSVGFactory.NewSvg;
SVG.LoadFromFile('icon.svg');
SVGIconImageList.Add(SVG, 'IconName');
```

### Working with the REST API

The library integrates with iconify.design API to search and download icons:
- Requires `SVGIconImageListRestClient` package
- Available in component editor "Add from WEB" option
- See `Browser.IconifyApi.pas` for API implementation
- Can filter by icon set collection

## Setup/Installer

The `Setup/` directory contains InnoSetup scripts for automatic installation:
- Detects installed Delphi versions
- Automatically builds packages for detected versions
- Installs packages and adds source paths
- Main script uses include files in `Setup/InnoSetupScripts/Source/`
- Build logic in `RADStudio.Build.inc`

## External Dependencies

### Image32 Library
- Location: `Image32/` directory (included)
- Author: Angus Johnson
- License: Boost Software License Version 1.0
- Version: 4.9 (as of latest update)
- Provides native Delphi SVG rendering

### Skia4Delphi (Optional)
- NOT included - must be installed separately
- Only needed if using Skia engine
- License: MIT
- Cross-platform 2D graphics based on Google's Skia

### SVGMagic Library
- Location: `SVGMagic/` directory (included)
- Author: Ursa Minor Ltd.
- Advanced SVG 1.1 rendering engine
- **Unique feature: Full support for animated SVG files**
- Comprehensive implementation with excellent rendering quality
- Available as optional engine via `{$DEFINE SVGMagic_SVGEngine}`

## Testing

Test projects located in `Test/` directory. The main test project demonstrates issue reproduction and validation.

## Related Projects

- **IconFontsImageList** - Similar library for icon fonts by Ethea: https://github.com/EtheaDev/IconFontsImageList
- **SVG Shell Extensions** - Windows Explorer integration: https://github.com/EtheaDev/SVGShellExtensions
