# RFishBC 0.0.3 ongoing
* `iGetImage()`: Modified. Streamlined code.

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
