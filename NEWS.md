# RFishBC 0.0.11.9000 ongoing
* Excluded many of the interactive lines from the coverage statistics. Updated other tests (aiming for comprehensiveness).
* Started a cran-comments document to being preparations for a CRAN release.
* Added `Encoding: UTF-8` to DESCRIPTION.
* `bcFuns()`: Modified. Fixed bug in `verbose=` result when `BCM=18`.
* `listFiles()`: Modified. Fixed bug related to `ignore.case=`.
* `getID()`: Modified. Slightly simplified code. Added tests.

# RFishBC 0.0.11 26-Jul-18
* Set TravisCI to auto-update the webpage documentation ([See this](https://www.datacamp.com/community/tutorials/cd-package-docs-pkgdown-travis)).
* Updated "Collect Radii Data" and the "Workflow" vignettes for changes below.
* Updated the webpage for changes below.
* `digitizeRadii()`: Modified. Moved the main code to `iDigitizeRadii1()`, which is basically the olde `digitizeRadii()` for working with only one image. The new `digitizeRadii()` allows `nm=` and `id=` to be vectors with length greater than 1 so that multiple images (and corresponding IDs) can be given to the function at once (or selected via a dialog box). Thus, this function now handles single or multiple images. This addresses [#20](https://github.com/droglenc/RFishBC/issues/20) (thanks to B. Utrup and J-M. Hessenauer). Also changed so that the dialog box for entering fish IDs is populated with a better guess at the fish's ID by using `getID()` with the new `IDpattern=`.
* `getID()`: Added. Part of addressing [#20](https://github.com/droglenc/RFishBC/issues/20).
* `RFBCoptions()`: Modified. Added `IDpattern=` (Part of addressing [#20](https://github.com/droglenc/RFishBC/issues/20)).

# RFishBC 0.0.10 3-July-18
* Added CITATION file.
* Added hex sticker.
* Added workflow vignette.
* `combineData()`: Modified. Allowed first argument to be an `RFishBC` object saved from `digitizeRadii()` (partially addresses [#16](https://github.com/droglenc/RFishBC/issues/16); thanks to [Jason Doll](https://github.com/jcdoll79)).
* `digitizeRadii()`: Modified. Added `closeWindow=` (addresses  [#14](https://github.com/droglenc/RFishBC/issues/14)). Changed so that the rownames of the `pts` data.frame in the returned object includes "center" and "edge." See changes to `iSelectPt()`.
* `findScalingFactor()`: Modified. Added `closeWindow=` (addresses  [#14](https://github.com/droglenc/RFishBC/issues/14)). See changes to `iSelectPt()`.
* `iSelectPt()`: Modified. Added `numPts=` with several "catches" for whether this target number of points is met when the "f" key (for "finished") was pressed. Alters behavior in `digitizeRadii()` and `findScalingFactor()` (addresses [#17](https://github.com/droglenc/RFishBC/issues/17); thanks to [Jason Doll](https://github.com/jcdoll79)).
* `RFBCoptions()`: Modified. Added `closeWindow=` (addresses  [#14](https://github.com/droglenc/RFishBC/issues/14)).
* `showDigitizedImage()`: Modified. Fixed bug in `iShowAnnuliLabels()` (addressed [#12](https://github.com/droglenc/RFishBC/issues/12); thanks to [Liuyong Ding](https://github.com/Otoliths)). Allowed first argument to be an `RFishBC` object saved from `digitizeRadii()` (partially addresses [#16](https://github.com/droglenc/RFishBC/issues/16); thanks to [Jason Doll](https://github.com/jcdoll79)). Changed `nm=` to `nms=` to be consistent with `combineData()`.

# RFishBC 0.0.9 18-May-18
* Added `rlang`, `stringr`, and `tidyr` to Imports.
* Added tests.
* Updated "Collect Data" vignette for changes to `combineData()`.
* Changed "Back-calculating Lengths" vignette for new `backCalc()`.
* `aStandard()`: Added. Addresses [#10](https://github.com/droglenc/RFishBC/issues/10).
* `backCalc()`: Added.
* `bcFuns()`: Modified. Changed to using `STOP()`. Changed all `Lc` to `Lcap`, `Rc` to `Rcap`, `agec` to `Acap`, and `agei` to `Ai`. Changed all BPH-related models to use `a`, `b`, and `c` and all SPH-related models to use `A`, `B`, and `C`. Started using `iGetBCMethod()`. Removed `verbose=` (moved to within the returned function).
* `bcUtilChecker()`: Modified. Changed to using `STOP()` and `WARN()`.
* `combineData()`: Modified. Added `outFormat=` and `deletePlusGrowth=` arguments (and corresponding tests).
* `findScalingFactor()`: Modified. Added a catch for non-positive `knownLength=`.
* `iGetBCMethod()`: Added. Used in `backCalc()` and `bcFuns()`.
* `SMBassWB1`: Added.
* `SMBassWB2`: Added.
* `StdIntLit`: Added.

# RFishBC 0.0.8 13-May-18
* Added tests.
* Changed to using `.rds` files rather than `.RData` files to save the data. This required using `saveRDS()` in `digitizeRadii()` and `readRDS()` in `showDigitizedImage()` and `combineData()`.
* `combineData()`: Modified. See note above. Added a check that the file is an "R Data" file (using `isRData()`) and of the `RFishBC` class.
* `digitizeRadii()`: Modified. See note above. Added the `RFishBC` class to the saved and returned object.
* `showDigitizedImage()`: Modified. See note above. Added a check that the file is an "R Data" file (using `isRData()`) and of the `RFishBC` class.
* `isRdata()`: Added.

# RFishBC 0.0.7 10-May-18
* Updated tests.
* `digitizeRadii()`: Modified. Added ability (using `iSelectPt()`) to delete points after selection for scale-bar, transect, and annuli selection. This removed the use of `locator()` and thus key-presses are used to terminate the selection of points. Added the `pch.del=` and `col.del=` arguments. Removed the `orig.pts` data.frame from the returned object.
* `findScalingFactor()`: Modified. Added ability (using `iSelectPt()`) to delete points after selection for scale-bar. Added `pch.sel=`, `col.sel=`, `cex.sel=`, `pch.del=`, and `col.del=` arguments.
* `RFBCoptions()`: Modified. Added the `pch.del=` and `col.del=` arguments. Removed `pch.show2=`, `col.show2=`, and `cex.show2=`
* `showDigitizedImage()`: Modified. Removed the ability to show the original (before snapping to the transect) points. Thus, removed `showOrigPts=`, `pch.show2=`, `col.show2=`, and `cex.show2=`.
* `iSelectPt()`: Added.

# RFishBC 0.0.6 9-May-18
* Added some tests.
* Added importFrom for `clisymbols` and `crayon` packages.
* `combineData()`: Modified. Added a check that the RData file has a `radii` object.
* `digitizeRadii()`: Modified. Fixed bug related to `showTransect=`. Added `clisymbols` to messages (and had to change `message()`s to `cat()`s). Added tests for the messages related to arguments (found early in the function).
* `iHndlFilenames()`: Modified. Slight modification of the error messages.

# RFishBC 0.0.5 29-Apr-18
* Changed license from MIT to GPL-3.
* `combineData()`: Modified. Better handles filenames (see `iHndlFilenames()`).
* `digitizeRadii()`: Modified. Better handles filenames (see `iHndlFilenames()`).
* `findScalingFactor()`: Modified. Better handles filenames (see `iHndlFilenames()`).
* `showDigitizedImage()`: Modified. Better handles filenames (see `iHndlFilenames()`). Streamlined the looping code. Made a catch for the situation where one of the multiple files selected does not appear to be derived from the same structure image as the first file.
* `iHndlFilenames()`: Modified. Streamlined and changed to using `choose.files()` to allow for more efficient selection of multiple files. Filtered the choices to image files or RData files. Now checks to see if the user is using Windows and if the selected files are in the current working directory.

# RFishBC 0.0.4 29-Apr-18
* Complete reworking of the code. The big difference is a requirement that the data files be in the current working directory.
* `digitizeRadii()`: Modified. Incorporated some of the internal files, completely rewored the list that is returned, dealt with working directory change.
* `findScalingFactor()`: Modified. Dealth with working directory chagne.
* `showDigitizedImages()`: Modified. Incorporated some of the internal files and dealt with working directory change.
* `iFindTransect()`: Deleted. Moved into `digitizeRadii()`.
* `iHndlID()`: Deleted. Moved into `digitizeRadii()`.
* `iHndlScalingFactor()`: Deleted.
* `iHndlScalingFactorFromScaleBar()`: Added.
* `iProcessAnnuli()`: Deleted. Moved into `digitizeRadii()`.
* `iPts2Rad()`: Added.
* `iSelectAnnuli()`: Deleted. Moved into `digitizeRadii()`.
* `iShowTransect()`: Deleted. Deleted. Moved into `digitizeRadii()` and `showDigitizedImage()`.

# RFishBC 0.0.3 28-Apr-18
* `digitizeRadii()`: Modified. Slight modification to the messages in the console. Re worked the code with `locator()` so that the points will be shown "snapped to the transect" if `snap2Transect=TRUE` (this addresses [#7](https://github.com/droglenc/RFishBC/issues/7)). Added an `orig.pts` data.frame to the RData object which contains the original (non-snapped to transect) points (which can be plotted with `showDigitizedImage()`). Changed `addTransect=` to `showTransect=` to make more similar to `showDigitezedImages()`.
* `RFBCoptions()`: Modified. Added `showAnnuliLabels=`, `col.ann=`, and `cex.ann=` for use in `showDigitizedImage()`. Changed defaults of mostly colors, pchs, and cexs.
* `showDigitizedImage()`: Modified. Added `showAnnuliLabels=`, `col.ann=`, and `cex.ann=` to shown annuli numbers when just one transect is shown. Added `showOrigPts=`, `pch.show2=`, `col.show2=`, and `cex.show2=` to handle including original points on the image.
* `iFindTransect()`: Added. Moved this code out of `iSelectAnnuli()`.
* `iGetImage()`: Modified. Streamlined code. Added `native=TRUE` to `read.bitmap()` call to send to the underlying functions as this is apparently more efficient when using `rasterImage()` (which this uses).
* `iOrderPts()`: Added. Made it easier to add annuli labels in `showDigitizedImage()`.
* `iSelectAnnuli()`: Modified. Now calls `iSelectTransect()`.
* `iShowAnnuliLabels()`: Added. Made it easier to add annuli labels in `showDigitizedImage()`.
* `ishowTransect()`: Added. Moved this code out of `iSelectAnnuli()`. Also called from `showDigitizedImage()`.

# RFishBC 0.0.2 27-Apr-18
* `digitizeRadii()`: Modified. Added `snap2Transect=` to address [#1](https://github.com/droglenc/RFishBC/issues/1). Modified how `fname=` was handled if missing (see `iHndlfname()`) and if selecting an image from outside of the current working directory. Allowed user to choose `id=` through a dialog box or a console prompt (see `iHndlID()`), which addresses [#2](https://github.com/droglenc/RFishBC/issues/2). Added `popID=` (again see `iHndlID()`). Fixed poor directions about the use of the escape key to terminate `locator()`. Added `showInfo=`, `pos.info=`, `cex.info=`, and `col.info=` to address [#6](https://github.com/droglenc/RFishBC/issues/6).
* `iGetImage()`: Added. Was `iReadImage()`. Calculated the pixel width-to-height ratio (from image dimensions) and returned in list.
* `iHndlfname()`: Added. Used in `digitizeRadii()`, `showDigitizedImage()`, and `findScalingFactor()`.
* `iHndlID()`: Added. Used in `digitizeRadii()`. Was modified from initial to include the image filename sans extension as the default ID in the Windows dialog box (when used with `digitizeRadii()`), which partially addresses [#5](https://github.com/droglenc/RFishBC/issues/5).
* `iHndlScalingFactor()`: Modified. Added `pixW2H=` argument to receive the pixel width-to-height ratio to adjust distance calculations for non-square images.
* `iPlaceText()`: Added. Used to address [#6](https://github.com/droglenc/RFishBC/issues/6).
* `iReadImage()`: Removed. Changed to `iGetImage()`.
* `iSnap2Transect()`: Added. Used in `digitizeRadii()`.
* `iProcessAnnuli()`: Modified. Added `pixW2H=` argument to receive the pixel width-to-height ratio to adjust distance calculations for non-square images.
* `RFBCoptions()`: Modified. Added `popID=` (see `iHndlID()`). Added `showInfo=`, `pos.info=`, `cex.inf=`, and `col.info=` (see `digitizeRadii()`).
* `showDigitizedData()`: Modified. Moved `file.choose()` for `fname=` out of argument list and into the main function code. Also modified to allow the user to select an object out of the current working directory (only works with a single file).

# RFishBC 0.0.1 16-Apr-18
* First version. Everything is new.
