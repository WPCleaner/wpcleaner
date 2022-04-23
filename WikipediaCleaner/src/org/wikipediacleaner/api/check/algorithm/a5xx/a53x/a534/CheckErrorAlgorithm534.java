/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a5xx.a53x.a534;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import org.apache.commons.lang3.StringUtils;
import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.LinterCategory;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementFunction;
import org.wikipediacleaner.api.data.PageElementImage;
import org.wikipediacleaner.api.data.PageElementImage.Parameter;
import org.wikipediacleaner.api.data.PageElementParameter;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.comment.CommentBuilder;
import org.wikipediacleaner.api.data.contents.magicword.ImageMagicWordType;
import org.wikipediacleaner.api.data.contents.magicword.MagicWord;
import org.wikipediacleaner.api.data.contents.magicword.MagicWordType;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;
import org.wikipediacleaner.api.data.PageElementTemplate;


/**
 * Algorithm for analyzing error 534 of check wikipedia project.
 * Error 534: Bogus image options (see [[Special:LintErrors/bogus-image-options]])
 */
public class CheckErrorAlgorithm534 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm534() {
    super("Bogus image options");
  }


  /**
   * Analyze a page to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param onlyAutomatic True if analysis could be restricted to errors automatically fixed.
   * @return Flag indicating if the error was found.
   */
  @Override
  public boolean analyze(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors, boolean onlyAutomatic) {
    if ((analysis == null) || (analysis.getPage() == null)) {
      return false;
    }

    // Analyze each image
    boolean result = analyzeImages(analysis, errors);

    // Analyze each gallery tag
    // TODO: result |= analyzeGalleryTags(analysis, errors);

    return result;
  }

  /**
   * Analyze a page to check if errors are present in images.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  /*public boolean analyzeGalleryTags(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors) {
    List<PageElementTag> galleryTags = analysis.getCompleteTags(WikiTagType.GALLERY);
    if (galleryTags.isEmpty()) {
      return false;
    }
    boolean result = false;
    GalleryTagAnalyzer analyzer = new GalleryTagAnalyzer(analysis.getWikiConfiguration());
    String contents = analysis.getContents();
    for (PageElementTag tag : galleryTags) {
      GalleryTag galleryTag = analyzer.analyze(tag, contents);
      for (GalleryTagLine line : galleryTag.getLines()) {
        if (StringUtils.isNotEmpty(line.getOptions())) {
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, line.getBeginIndex(), line.getEndIndex(), ErrorLevel.WARNING);
          errors.add(errorResult);
        }
      }
    }
    return result;
  }*/

  /**
   * Analyze a page to check if errors are present in images.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  public boolean analyzeImages(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors) {
    List<PageElementImage> images = analysis.getImages();
    if (images.isEmpty()) {
      return false;
    }
    boolean result = false;
    List<Parameter> params = new ArrayList<>();
    List<Parameter> paramsFormat = new ArrayList<>();
    List<Parameter> paramsHAlign = new ArrayList<>();
    List<Parameter> paramsVAlign = new ArrayList<>();
    Map<MagicWordType, List<Parameter>> paramsOther = new HashMap<>();
    for (PageElementImage image : images) {

      // Categorize all parameters of the image
      groupParameters(image.getParameters(), params, paramsFormat, paramsHAlign, paramsVAlign, paramsOther);

      // Report images with several parameters that can't be related to a magic word
      result |= reportSeveralDescriptions(analysis, errors, image, params);

      // Report multiple options for several group of options
      result |= reportMultipleParameters(analysis, errors, image, paramsFormat, false);
      result |= reportMultipleParameters(analysis, errors, image, paramsHAlign, false);
      result |= reportMultipleParameters(analysis, errors, image, paramsVAlign, false);
      for (List<Parameter> paramOther : paramsOther.values()) {
        result |= reportMultipleParameters(analysis, errors, image, paramOther, params.isEmpty());
      }

      // Report special values for several group of options
      result |= reportIncorrectValue(analysis, errors, image, paramsFormat);
      result |= reportIncorrectValue(analysis, errors, image, paramsHAlign);
      result |= reportIncorrectValue(analysis, errors, image, paramsVAlign);
      for (List<Parameter> paramOther : paramsOther.values()) {
        result |= reportIncorrectValue(analysis, errors, image, paramOther);
      }
    }
    return result;
  }

  /**
   * Group image parameters by their category.
   * 
   * @param imageParameters List of image parameters.
   * @param params List of parameters without a related magic word.
   * @param paramsFormat List of parameters related to format options.
   * @param paramsHAlign List of parameters related to horizontal alignment options.
   * @param paramsVAlign List of parameters related to vertical alignment options.
   * @param paramsOther Map of other parameters grouped by their related magic word.
   */
  private void groupParameters(
      Collection<Parameter> imageParameters,
      List<Parameter> params,
      List<Parameter> paramsFormat,
      List<Parameter> paramsHAlign,
      List<Parameter> paramsVAlign,
      Map<MagicWordType, List<Parameter>> paramsOther) {

    // Clean lists
    params.clear();
    paramsFormat.clear();
    paramsHAlign.clear();
    paramsVAlign.clear();
    paramsOther.clear();
    if (imageParameters == null) {
      return;
    }

    // Classify each parameter
    for (Parameter param : imageParameters) {
      if (param != null) {
        MagicWord mw = param.getMagicWord();
        if (mw == null) {
          // NO magic word: description or unknown
          params.add(param);
        } else {
          MagicWordType mwType = mw.getType();
          // Format option: one of border and/or frameless, frame, thumb (or thumbnail)
          if (ImageMagicWordType.FORMAT_OPTIONS.contains(mwType)) {
            if (ImageMagicWordType.IMG_BORDER.equals(mwType)) {
              if (paramsFormat.isEmpty() ||
                  !ImageMagicWordType.IMG_FRAMELESS.equals(paramsFormat.get(0).getMagicWord().getType())) {
                paramsFormat.add(param);
              }
            } else
            if (ImageMagicWordType.IMG_FRAMELESS.equals(mwType)) {
              if (paramsFormat.isEmpty() ||
                  !ImageMagicWordType.IMG_BORDER.equals(paramsFormat.get(0).getMagicWord().getType())) {
                paramsFormat.add(param);
              }
            } else
            if (ImageMagicWordType.IMG_FRAMED.equals(mwType) ||
                ImageMagicWordType.IMG_THUMBNAIL.equals(mwType)) {
              if (paramsFormat.isEmpty() ||
                  !ImageMagicWordType.IMG_BORDER.equals(paramsFormat.get(paramsFormat.size() - 1).getMagicWord().getType())) {
                paramsFormat.add(param);
              } else {
                paramsFormat.add(paramsFormat.size() - 1, param);
              }
            }
          } else
          // Horizontal alignment option: one of left, right, center, none
          if (ImageMagicWordType.HORIZONTAL_ALIGN_OPTIONS.contains(mwType)) {
            paramsHAlign.add(param);
          } else
          // Vertical alignment option: one of baseline, sub, super, top, text-top, middle, bottom, text-bottom
          if (ImageMagicWordType.VERTICAL_ALIGN_OPTIONS.contains(mwType)) {
            paramsVAlign.add(param);
          } else {
            List<Parameter> tmpList = paramsOther.get(mwType);
            if (tmpList == null) {
              tmpList = new ArrayList<>();
              paramsOther.put(mwType, tmpList);
            }
            tmpList.add(param);
          }
        }
      }
    }
  }

  /**
   * Report errors for incorrect value.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param image Image being analyzed.
   * @param params List of parameters.
   * @return Flag indicating if the error was found.
   */
  private boolean reportIncorrectValue(
      PageAnalysis analysis, Collection<CheckErrorResult> errors,
      PageElementImage image, List<Parameter> params) {
    boolean result = false;
    for (Parameter param : params) {
      if ((param.getMagicWord() != null) &&
          ImageMagicWordType.IMG_UPRIGHT.equals(param.getMagicWord().getType())) {
        result |= reportUprightIncorrectValue(analysis, errors, image, param);
      }
    }
    return result;
  }

  private static final Pattern UPRIGHT_PATTERN = Pattern.compile("\\d++(?:\\.\\d++)?");

  /**
   * Report errors for incorrect value in upright parameter.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param image Image being analyzed.
   * @param param Parameters.
   * @return Flag indicating if the error was found.
   */
  private boolean reportUprightIncorrectValue(
      PageAnalysis analysis, Collection<CheckErrorResult> errors,
      PageElementImage image, Parameter param) {

    // Check if the value is correct
    String paramContents = param.getContents();
    int equalIndex = paramContents.indexOf("=");
    if (equalIndex < 0) {
      // We don't know how to analyze the value, so we suppose it's correct
      return false;
    }
    String value = paramContents.substring(equalIndex + 1);
    if (UPRIGHT_PATTERN.matcher(value).matches()) {
      return false;
    }

    // Report error
    if (errors == null) {
      return true;
    }
    int beginIndex = image.getBeginIndex() + param.getBeginOffset();
    int endIndex = image.getBeginIndex() + param.getEndOffset();
    CheckErrorResult errorResult = createCheckErrorResult(
        analysis, beginIndex, endIndex, ErrorLevel.ERROR);
    String newValue = value.replaceAll(",", ".").trim();
    if (newValue.isEmpty()) {
      errorResult.addReplacement("", true);
    } else if (UPRIGHT_PATTERN.matcher(newValue).matches()) {
      errorResult.addReplacement(
          paramContents.substring(0, equalIndex).trim() + "=" + newValue,
          true);
    }
    errors.add(errorResult);
    return true;
  }

  /**
   * Report errors for multiple descriptions.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param image Image being analyzed.
   * @param params List of parameters for the description.
   * @return Flag indicating if the error was found.
   */
  private boolean reportSeveralDescriptions(
      PageAnalysis analysis, Collection<CheckErrorResult> errors,
      PageElementImage image, List<Parameter> params) {
    
    if (params.size() < 2) {
      return false;
    }
    if (errors == null) {
      return true;
    }

    // Analyze if there's a risk of error
    boolean safe = true;
    boolean safeEmpty = true;
    String contents = analysis.getContents();
    String imageText = contents.substring(image.getBeginIndex(), image.getEndIndex());
    int lastParamIndex = image.getBeginIndex() + params.get(params.size() - 1).getBeginOffset();
    for (int index = 0; (index < imageText.length() - 1) && safeEmpty; index++) {
      char currentChar = imageText.charAt(index);
      if (currentChar == '{') {
        char nextChar = imageText.charAt(index + 1);
        if (nextChar == '|') {
          safe = false;
          safeEmpty = false;
        } else if (safe && (nextChar == '{')) {
          // TODO: analyze templates/functions/...
          PageElementTemplate template = analysis.isInTemplate(image.getBeginIndex() + index);
          PageElementFunction function = analysis.isInFunction(image.getBeginIndex() + index);
          PageElementParameter parameter = analysis.isInParameter(image.getBeginIndex() + index);
          if ((template != null) || (function != null) || (parameter != null)) {
            index++;
            safe = false;
          } else {
            safe = false;
          }
        }
      } else if (currentChar == '<') {
        PageElementTag tag = analysis.isInTag(image.getBeginIndex() + index);
        if (tag != null) {
          if (WikiTagType.CHEM.equals(tag.getType()) ||
              WikiTagType.HIERO.equals(tag.getType()) ||
              WikiTagType.MATH.equals(tag.getType()) ||
              WikiTagType.MATH_CHEM.equals(tag.getType()) ||
              WikiTagType.NOWIKI.equals(tag.getType()) ||
              WikiTagType.REF.equals(tag.getType()) ||
              WikiTagType.SCORE.equals(tag.getType()) ||
              WikiTagType.SOURCE.equals(tag.getType()) ||
              WikiTagType.SYNTAXHIGHLIGHT.equals(tag.getType())) {
            safe = false;
            if (tag.getBeginIndex() <= lastParamIndex) {
              safeEmpty = false;
            }
          }
        }
      }
    }

    // Case when last parameter is empty
    Parameter param = params.get(params.size() - 1);
    int beginIndex = image.getBeginIndex() + param.getBeginOffset();
    int endIndex = image.getBeginIndex() + param.getEndOffset();
    boolean hasContents = false;
    for (int index = beginIndex; (index < endIndex) && !hasContents; index++) {
      if (contents.charAt(index) != ' ') {
        hasContents = true;
      }
    }
    if (!hasContents) {
      CheckErrorResult errorResult = createCheckErrorResult(
          analysis, beginIndex - 1, endIndex,
          safe ? ErrorLevel.ERROR : ErrorLevel.WARNING);
      errorResult.addReplacement("", false);
      errors.add(errorResult);
      return true;
    }

    // Check when last parameter is not empty
    for (int numParam = 0; numParam < params.size() - 1; numParam++) {
      param = params.get(numParam);
      beginIndex = image.getBeginIndex() + param.getBeginOffset();
      endIndex = image.getBeginIndex() + param.getEndOffset();
      hasContents = false;
      for (int index = beginIndex; (index < endIndex) && !hasContents; index++) {
        if (contents.charAt(index) != ' ') {
          hasContents = true;
        }
      }
      CheckErrorResult errorResult = createCheckErrorResult(
          analysis, beginIndex - 1, endIndex);
      if (!hasContents) {
        PageElementTemplate template = analysis.isInTemplate(beginIndex);
        if ((template != null) && (template.getBeginIndex() > image.getBeginIndex())) {
          safeEmpty = false;
        }
        PageElementParameter parameter = analysis.isInParameter(beginIndex);
        if ((parameter != null) && (parameter.getBeginIndex() > image.getBeginIndex())) {
          safeEmpty = false;
        }
        errorResult.addReplacement("", safeEmpty);
      } else if (isImageNameInParameter(image, param)) {
        errorResult.addReplacement("", safe);
      } else {
        AutomaticReplacement replacement = AutomaticReplacement.suggestReplacement(
            analysis.getWikiConfiguration(),
            param, image.getParameters());
        if (replacement != null) {
          String text = replacement.targetText;
          if ((text != null) && !text.isEmpty()) {
            errorResult.addReplacement("|" + replacement.targetText, safe && replacement.automatic);
          } else {
            errorResult.addReplacement("", safe && replacement.automatic);
          }
        }
      }
      errors.add(errorResult);
    }

    // Report last parameter as correct
    param = params.get(params.size() - 1);
    beginIndex = image.getBeginIndex() + param.getBeginOffset();
    endIndex = image.getBeginIndex() + param.getEndOffset();
    CheckErrorResult errorResult = createCheckErrorResult(
        analysis, beginIndex, endIndex, ErrorLevel.CORRECT);
    errors.add(errorResult);

    return true;
  }

  /**
   * @param image Image.
   * @param param Parameter.
   * @return True if parameter text is a match with the image name.
   */
  private boolean isImageNameInParameter(PageElementImage image, Parameter param) {
    String imageName = image.getImage();
    String paramValue = param.getContents();
    if (StringUtils.equalsIgnoreCase(imageName, paramValue)) {
      return true;
    }
    int colonIndex = imageName.lastIndexOf('.');
    if ((colonIndex > 0) &&
        StringUtils.equalsIgnoreCase(imageName.substring(0, colonIndex), paramValue)) {
      return true;
    }
    return false;
  }

  /**
   * Report errors for multiple parameters of the same kind.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param image Image being analyzed.
   * @param params List of parameters of the same kind.
   * @return Flag indicating if the error was found.
   */
  private boolean reportMultipleParameters(
      PageAnalysis analysis, Collection<CheckErrorResult> errors,
      PageElementImage image, List<Parameter> params,
      boolean mayRemoveOne) {
    if ((params == null) || (params.size() < 2)) {
      return false;
    }
    if (mayRemoveOne && (params.size() < 3)) {
      return false;
    }
    if (errors == null) {
      return true;
    }
    boolean keepFirst = true;
    boolean useComments = false;
    Parameter paramKeep = params.get(0);
    if (paramKeep.getMagicWord() != null) {
      if (ImageMagicWordType.IMG_ALT.equals(paramKeep.getMagicWord().getType())) {
        keepFirst = false;
        useComments = true;
        paramKeep = params.get(params.size() - 1);
      } else if (ImageMagicWordType.IMG_UPRIGHT.equals(paramKeep.getMagicWord().getType())) {
        keepFirst = false; // Due to MW processing upright parameters in a different order: T216003
      } else if (ImageMagicWordType.IMG_WIDTH.equals(paramKeep.getMagicWord().getType())) {
        keepFirst = false; // Tests show that the last size is kept, not the first one
      }
    }
    int beginIndexKeep = image.getBeginIndex() + paramKeep.getBeginOffset();
    int endIndexKeep = image.getBeginIndex() + paramKeep.getEndOffset();
    for (int numParam = 1; numParam < params.size(); numParam++) {
      Parameter param = params.get(keepFirst ? numParam : numParam - 1);
      int beginIndex = image.getBeginIndex() + param.getBeginOffset();
      int endIndex = image.getBeginIndex() + param.getEndOffset();

      // Check if modifications can be automatic
      boolean automatic = true;
      if (analysis.isInFunction(beginIndex) != null) {
        automatic = false;
      }
      ErrorLevel errorLevel = ErrorLevel.ERROR;
      if (!paramKeep.getCorrect()) {
        if ((numParam == 1) && param.getCorrect()) {
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, beginIndexKeep - 1, endIndexKeep);
          errorResult.addReplacement("", automatic);
          errors.add(errorResult);
          errorLevel = ErrorLevel.WARNING;
        }
        if (param.getCorrect()) {
          automatic = false;
        }
      }
      if (automatic) {
        if ((analysis.isInParameter(beginIndex) != null) ||
            (analysis.isInFunction(beginIndex) != null)) {
          automatic = false;
        }
      }

      // Add error
      CheckErrorResult errorResult = createCheckErrorResult(
          analysis, beginIndex - 1, endIndex, errorLevel);
      errorResult.addReplacement("", automatic);
      errors.add(errorResult);

      // Handle comments
      if (useComments) {
        if (!param.getCorrect()) {
          useComments = false;
        }
        if (StringUtils.equals(param.getContents(), paramKeep.getContents())) {
          useComments = false;
        }
      }
      if (useComments) {
        errorResult = createCheckErrorResult(analysis, beginIndexKeep - 1, endIndexKeep);
        String contents = analysis.getContents();
        String replacement =
            contents.substring(beginIndexKeep - 1, endIndexKeep) +
            CommentBuilder.from(contents.substring(beginIndex, endIndex)).toString();
        errorResult.addReplacement(replacement, automatic);
        errors.add(errorResult);
      }
    }
    return true;
  }

  /**
   * Automatic fixing of some errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalAutomaticFix(PageAnalysis analysis) {
    return fixUsingAutomaticReplacement(analysis);
  }

  /**
   * @return True if the error has a special list of pages.
   */
  @Override
  public boolean hasSpecialList() {
    return (linterCategory != null);
  }

  /**
   * Retrieve the list of pages in error.
   * 
   * @param wiki Wiki.
   * @param limit Maximum number of pages to retrieve.
   * @return List of pages in error.
   */
  @Override
  public List<Page> getSpecialList(EnumWikipedia wiki, int limit) {
    List<Page> result = null;
    if (linterCategory != null) {
      API api = APIFactory.getAPI();
      try {
        result = api.retrieveLinterCategory(
            wiki, linterCategory.getCategory(),
            Namespace.MAIN, false, true, limit);
      } catch (APIException e) {
        //
      }
    }
    return result;
  }

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    List<LinterCategory> categories = getWikiConfiguration().getLinterCategories();
    if (categories != null) {
      for (LinterCategory category : categories) {
        if ("bogus-image-options".equals(category.getCategory())) {
          linterCategory = category;
        }
      }
    }
  }

  /** Linter category */
  private LinterCategory linterCategory = null;
}
