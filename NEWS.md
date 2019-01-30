# RFishBC 0.2.0.9000 ongoing
* Updated vignettes.
* `combineData()`: Modified. Fixed two bugs related to how age-0 fish with no measured annuli are combined (thanks to Rory Feeney). Fixed how returned results are sorted (by id, reading, and ann; rather than just id and ann).
* `digitizeRadii()`: Modified. Added `sbUnits=` for addressing [#36](https://github.com/droglenc/RFishBC/issues/36). Fixed poor warning message when `snap2transect=TRUE` and `makeTransect=FALSE`.
* `RFBCoptions()`: Modified. Added `showScaleBarLength=`, `scaleBarUnits=`, and `cex.scaleBar=`.
* `showDigitizedImage()`: Modified. Rewrote the function to be less redundant; allow `pch.show=`, `col.show=`, and `cex.show=` to appropriate vectors (when only one reading is displayed); assure that all values in `annuliLabels=` exist in the data; allow `col.ann=` and `cex.ann` to be appropriate vectors; provide `offset.ann=`; and show units of the scale-bar beneath the scale-bar (if it was digitized) by adding `showScaleBarLength=` and `cex.scaleBar=` (addresses [#36](https://github.com/droglenc/RFishBC/issues/36)).  

# RFishBC 0.2.0 18-Dec-18
* Added several items (mostly images) to .Rbuildignore to lower bloat of package.
* Added .Rbuildignore to remove sticker folder from package directory.
* Added validation tests (still need to be run manually) from Sullivan data.
* `combineData()`: Modified. Changed `outFormat=` to `formatOut=`. Added `outType=` to allow user to choose either radial or incremental measurements as output.
* `digitizeRadii()`: Modified. Changed `showTransect=` to `makeTransect`. Changed so that `snap2Transect` is changed to `FALSE` if it is `TRUE` and `makeTransect=FALSE`. Radii are now computed as the cumulative sum of increments rather than the distance from the selected point back to the structure center. This addresses [#32](https://github.com/droglenc/RFishBC/issues/32) [Thanks to Alan Hanke].
* `RFBCoptions()`: Modified. Now includes `makeTransect=` used in `digitizeRadii()`. Now includes `connect=`, `col.connect=`, and `lwd.connect=` used in `showDigitizedImage()`. Because of those changes `showTransect=` has been removed.
* `showDigitizedImage()`: Modified. Changed `showTransect=` to `connect=`, `col.transect=`  to `col.connect=`, and `lwd.transect=` to `lwd.connect=` (this helps address [#32](https://github.com/droglenc/RFishBC/issues/32)) [Thanks to Alan Hanke]. Also removed the points that are plotted at the structure center and, if not an annulus, margin (this addresses [#31](https://github.com/droglenc/RFishBC/issues/31)). Made more robust checks for when the user tries to plot two images from different structures.

# RFishBC 0.1.2 9-Dec-18
* Added dependency to `cli` package for `DONE()`, `NOTE()`, and `RULE()` (see below).
* `digitizeRadii()`: Modified. Changed console output directions to use `RULE()` instead of `NOTE()`. Fixed bug that produced error related to closing the last window when working with multiple images.
* `DONE()`: Modified. Changed to use `cat_line()` from `cli` package (removed my hack).
* `findScalingFactor()`: Modified. Changed console output directions to use `RULE()` instead of `NOTE()`.
* `NOTE()`: Modified. Changed to use `cat_line()` from `cli` package (removed my hack).
* `RULE()`: Added.

# RFishBC 0.1.1 12-Nov-18
* Released to CRAN.
* Removed automatic webpage construction from Travis-CI. Did not work with some of the updated vignettes.
* Put fishBC in the Description field into single quotes as directed by CRAN.
* `digitizeRadii()`. Modified. Added a note that draws attention when `snap2Transect=TRUE` and `showTransect=FALSE` (i.e., "snapping" to a transect that is not shown). At least partially addresses [#30](https://github.com/droglenc/RFishBC/issues/30).

# RFishBC 0.1.0 7-Nov-18
* `iSnap2Transect()`: Modified. Changed to handle bugs related to perfectly vertical or perfectly horizontal transects. Addresses [#27](https://github.com/droglenc/RFishBC/issues/27) (Thanks to Ben Neely).
* `digitizeRadii()`: Modified. Removed `sepWindow()`. Added `deviceType=`.
* `findScalingFactor()`: Modified. Removed `sepWindow()`. Added `deviceType=`.
* `getID()`: Modified. Added `IDreplace=`. Added more examples of use of `IDpattern=` and `IDreplace=` and more tests.
* `iGetimage()`: Modified. Removed `sepWindow=`. Added `deviceType=`.
* `RFBCoptions()`: Modified. Removed `sepWindow()`. Added `deviceType=` and `IDreplace=`.
* `showDigitizedImage()`: Modified. Removed `closeWindow=` and `sepWindow()`.

# RFishBC 0.0.13 6-Oct-18
* `combineData()`: Modified. Changed so that an age-0 fish with all plus-growth is still included in the returned data.frame when `deletePlusGrowth=TRUE` (the `ann` and `rad` variables will both be `NA`). Thanks to Ben Neely.
* `digitizeRadii()`: Modified. Removed restriction that one point be selected as an annulus, which allows for handling age-0 fish (addresses [#25](https://github.com/droglenc/RFishBC/issues/25); thanks to Ben Neely for the suggestion). Removed the `q` and `r` button equivalencies for `f` and `d`. Added the ability for the user to start over (resulting in no file to be written, but the current image stays live; uses the `z` key) (addresses [#22](https://github.com/droglenc/RFishBC/issues/22); thanks to Ben Neely for the suggestion). Added the ability for the user to abort a processing (resulting in no file to be written and moving to the next image if using multiple images; uses the `q` key) (addresses [#24](https://github.com/droglenc/RFishBC/issues/24); thanks to Ben Neely for the suggestion).
* `findScalingFactor()`: Modified. Changed to allow the user to abort or restart the process, similar to for `digitizeRadii()`.

# RFishBC 0.0.12 3-Oct-18
* Excluded many of the interactive lines from the coverage statistics. Updated other tests (aiming for comprehensiveness).
* Started a cran-comments document to begin preparations for a CRAN release.
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
