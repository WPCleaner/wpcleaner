/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.constants.WikiConfiguration;
import org.wikipediacleaner.api.data.MagicWord;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementImage;
import org.wikipediacleaner.api.data.PageElementImage.Parameter;


/**
 * Algorithm for analyzing error 534 of check wikipedia project.
 * Error 534: Bogus image options (see [[Special:LintErrors/bogus-image-options]])
 */
public class CheckErrorAlgorithm534 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm534() {
    super("Bogus image options");
  }

  /** Bean for holding automatic replacements */
  public static class AutomaticReplacement {

    /** Initial text */
    public final String initialText;

    /** Target magic word */
    public final String targetMagicWord;

    /** Target text */
    public final String targetText;

    /** True if replacement can be done automatically */
    public final boolean automatic;

    /** List of magic words with potential options */
    private final static String[] mwOptions = {
      MagicWord.IMG_ALT,
      MagicWord.IMG_BASELINE,
      MagicWord.IMG_BORDER,
      MagicWord.IMG_BOTTOM,
      MagicWord.IMG_CENTER,
      MagicWord.IMG_CLASS,
      MagicWord.IMG_FRAMED,
      MagicWord.IMG_FRAMELESS,
      MagicWord.IMG_LANG,
      MagicWord.IMG_LEFT,
      MagicWord.IMG_LINK,
      MagicWord.IMG_LOSSY,
      MagicWord.IMG_MANUAL_THUMB,
      MagicWord.IMG_MIDDLE,
      MagicWord.IMG_NONE,
      MagicWord.IMG_PAGE,
      MagicWord.IMG_RIGHT,
      MagicWord.IMG_SUB,
      MagicWord.IMG_SUPER,
      MagicWord.IMG_TEXT_BOTTOM,
      MagicWord.IMG_TEXT_TOP,
      MagicWord.IMG_THUMBNAIL,
      MagicWord.IMG_TOP,
      MagicWord.IMG_UPRIGHT,
      MagicWord.IMG_WIDTH,
    };

    /**
     * @param initialText Initial text.
     * @param targetMagicWord Target magic word.
     * @param targetText Target text.
     * @param automatic True if replacement can be done automatically.
     */
    public AutomaticReplacement(
        String initialText,
        String targetMagicWord, String targetText,
        boolean automatic) {
      this.initialText = initialText;
      this.targetMagicWord = targetMagicWord;
      this.targetText = targetText;
      this.automatic = automatic;
    }

    /**
     * @param replacements List of automatic replacements.
     * @param config Wiki configuration.
     * @param param Current parameter.
     * @param params All parameters.
     * @return Replacement for the initial text.
     */
    public static AutomaticReplacement suggestReplacement(
        AutomaticReplacement[] replacements,
        WikiConfiguration config,
        Parameter param, Collection<Parameter> params) {

      // Check parameters
      if ((replacements == null) ||
          (config == null) ||
          (param == null) ||
          (param.getContents() == null)) {
        return null;
      }
      String initialText = param.getContents().trim();

      // Find a suggestion
      AutomaticReplacement replacement = findSuggestion(
          replacements, config, initialText);

      // Handle specifically a second description
      if ((replacement == null) && (param.getMagicWord() == null)) {
        String secondDescription = null;
        boolean hasAltDescription = false;
        for (Parameter paramTmp : params) {
          if ((paramTmp != param) && (paramTmp.getMagicWord() == null)) {
            secondDescription = paramTmp.getContents();
          }
          if ((paramTmp.getMagicWord() != null) &&
              MagicWord.IMG_ALT.equals(paramTmp.getMagicWord().getName())) {
            hasAltDescription = true;
          }
        }
        if (secondDescription != null) {
          if (secondDescription.equals(initialText)) {
            return new AutomaticReplacement(initialText, null, null, true);
          }
          if (!hasAltDescription) {
            String targetText = "alt=" + initialText;
            MagicWord alt = config.getMagicWordByName(MagicWord.IMG_ALT);
            if ((alt != null) && (alt.isPossibleAlias(targetText))) {
              return new AutomaticReplacement(
                  initialText, MagicWord.IMG_ALT, targetText, false);
            }
          }
        }
      }
      if (replacement == null) {
        return null;
      }

      // Check that the suggestion can be applied
      if (replacement.targetMagicWord != null){
        String mwName = replacement.targetMagicWord;
        boolean paramFound = false;
        for (Parameter paramTmp : params) {
          if (paramTmp == param) {
            paramFound = true;
          } else if (paramTmp.getMagicWord() != null) {
            // If option already exists, remove the faulty one
            String mwNameTmp = paramTmp.getMagicWord().getName();
            if (mwName.equals(mwNameTmp)) {
              return new AutomaticReplacement(initialText, null, null, true);
            }

            // Format option: one of border and/or frameless, frame, thumb (or thumbnail)
            if (MagicWord.IMG_BORDER.equals(mwName) ||
                MagicWord.IMG_FRAMELESS.equals(mwName) ||
                MagicWord.IMG_FRAMED.equals(mwName) ||
                MagicWord.IMG_THUMBNAIL.equals(mwName)) {
              if (MagicWord.IMG_BORDER.equals(mwNameTmp) ||
                  MagicWord.IMG_FRAMELESS.equals(mwNameTmp) ||
                  MagicWord.IMG_FRAMED.equals(mwNameTmp) ||
                  MagicWord.IMG_THUMBNAIL.equals(mwNameTmp)) {
                return new AutomaticReplacement(initialText, null, null, !paramFound);
              }
            } else
            // Horizontal alignment option: one of left, right, center, none
            if (MagicWord.IMG_CENTER.equals(mwName) ||
                MagicWord.IMG_LEFT.equals(mwName) ||
                MagicWord.IMG_NONE.equals(mwName) ||
                MagicWord.IMG_RIGHT.equals(mwName)) {
              if (MagicWord.IMG_CENTER.equals(mwNameTmp) ||
                  MagicWord.IMG_LEFT.equals(mwNameTmp) ||
                  MagicWord.IMG_NONE.equals(mwNameTmp) ||
                  MagicWord.IMG_RIGHT.equals(mwNameTmp)) {
                return new AutomaticReplacement(initialText, null, null, !paramFound);
              }
            } else
            // Vertical alignment option: one of baseline, sub, super, top, text-top, middle, bottom, text-bottom
            if (MagicWord.IMG_BASELINE.equals(mwName) ||
                MagicWord.IMG_BOTTOM.equals(mwName) ||
                MagicWord.IMG_MIDDLE.equals(mwName) ||
                MagicWord.IMG_SUB.equals(mwName) ||
                MagicWord.IMG_SUPER.equals(mwName) ||
                MagicWord.IMG_TEXT_BOTTOM.equals(mwName) ||
                MagicWord.IMG_TEXT_TOP.equals(mwName) ||
                MagicWord.IMG_TOP.equals(mwName)) {
              if (MagicWord.IMG_BASELINE.equals(mwName) ||
                  MagicWord.IMG_BOTTOM.equals(mwName) ||
                  MagicWord.IMG_MIDDLE.equals(mwName) ||
                  MagicWord.IMG_SUB.equals(mwName) ||
                  MagicWord.IMG_SUPER.equals(mwName) ||
                  MagicWord.IMG_TEXT_BOTTOM.equals(mwName) ||
                  MagicWord.IMG_TEXT_TOP.equals(mwName) ||
                  MagicWord.IMG_TOP.equals(mwName)) {
                return new AutomaticReplacement(initialText, null, null, !paramFound);
              }
            }
          }
        }
      }

      return replacement;
    }

    /**
     * @param replacements List of automatic replacements.
     * @param config Wiki configuration.
     * @param initialText Initial text.
     * @return Replacement for the initial text.
     */
    private static AutomaticReplacement findSuggestion(
        AutomaticReplacement[] replacements,
        WikiConfiguration config,
        String initialText) {

      // Find a direct suggestion
      for (AutomaticReplacement replacement : replacements) {
        if (initialText.equals(replacement.initialText)) {
          if (replacement.targetMagicWord == null) {
            return replacement;
          }
          MagicWord magicWord = config.getMagicWordByName(replacement.targetMagicWord);
          if ((magicWord != null) && (magicWord.isPossibleAlias(replacement.targetText))) {
            return replacement;
          }
        }
      }

      // Find a suggestion ignoring case
      for (AutomaticReplacement replacement : replacements) {
        if (initialText.equalsIgnoreCase(replacement.initialText)) {
          if (replacement.targetMagicWord == null) {
            return replacement;
          }
          MagicWord magicWord = config.getMagicWordByName(replacement.targetMagicWord);
          if ((magicWord != null) && (magicWord.isPossibleAlias(replacement.targetText))) {
            return replacement;
          }
        }
      }

      // Find various suggestions
      for (String mwName : mwOptions) {
        MagicWord mw = config.getMagicWordByName(mwName);
        if ((mw != null) && (mw.getAliases() != null)) {
          for (String alias : mw.getAliases()) {
            int variablePos = alias.indexOf("$1");
            if (variablePos < 0) {
              if (initialText.equalsIgnoreCase(alias)) {
                return new AutomaticReplacement(initialText, mwName, alias, true);
              }
            } else {

              // Check possibility to use the option
              boolean ok = true;
              final int prefixLength = variablePos;
              int newPrefixLength = prefixLength;
              final int suffixLength = alias.length() - 2 - variablePos;
              int newSuffixLength = suffixLength;
              if (initialText.length() < newPrefixLength + newSuffixLength) {
                ok = false;
              } else {
                int equalIndex = alias.indexOf('=');
                if (newPrefixLength > 0) {
                  // Check what is before the variable
                  if (equalIndex == variablePos - 1) {
                    if (!initialText.substring(0, equalIndex).equalsIgnoreCase(alias.substring(0, equalIndex))) {
                      ok = false;
                    } else {
                      int tmpIndex = equalIndex;
                      while ((tmpIndex < initialText.length()) &&
                             (initialText.charAt(tmpIndex) == ' ')) {
                        tmpIndex++;
                      }
                      if ((tmpIndex >= initialText.length()) ||
                          ("=:-)(".indexOf(initialText.charAt(tmpIndex)) < 0)) {
                        ok = false;
                      } else {
                        tmpIndex++;
                      }
                      while ((tmpIndex < initialText.length()) &&
                             (initialText.charAt(tmpIndex) == ' ')) {
                        tmpIndex++;
                      }
                      newPrefixLength = tmpIndex;
                    }
                  } else {
                    if (!initialText.substring(0, newPrefixLength).equalsIgnoreCase(alias.substring(0, newPrefixLength))) {
                      ok = false;
                    }
                  }
                }
                if (newSuffixLength > 0) {
                  // Check what is after the variable
                  String initialSuffix = initialText.substring(initialText.length() - newSuffixLength);
                  String aliasSuffix = alias.substring(alias.length() - newSuffixLength);
                  boolean suffixOk = false;
                  if (initialSuffix.equalsIgnoreCase(aliasSuffix)) {
                    suffixOk = true;
                  } else if (aliasSuffix.equals("px")) {
                    int lastDigit = 0;
                    while (((lastDigit) < initialText.length()) &&
                           (Character.isDigit(initialText.charAt(lastDigit)))) {
                      lastDigit++;
                    }
                    if (lastDigit > 0) {
                      String currentSuffix = initialText.substring(lastDigit);
                      if (currentSuffix.equalsIgnoreCase("p") ||
                          currentSuffix.equalsIgnoreCase("x") ||
                          currentSuffix.equalsIgnoreCase("px") ||
                          currentSuffix.equalsIgnoreCase("xp")) {
                        suffixOk = true;
                        newSuffixLength = currentSuffix.length();
                      } else if ((currentSuffix.length() == 2) &&
                                 ((Character.toLowerCase(currentSuffix.charAt(0)) == 'p') ||
                                  (Character.toLowerCase(currentSuffix.charAt(1)) == 'x'))) {
                        suffixOk = true;
                        newSuffixLength = currentSuffix.length();
                      } else if ((currentSuffix.length() == 3) &&
                                 (Character.toLowerCase(currentSuffix.charAt(0)) == 'p') &&
                                 (Character.toLowerCase(currentSuffix.charAt(2)) == 'x')) {
                        suffixOk = true;
                        newSuffixLength = currentSuffix.length();
                      }
                    }
                  }
                  ok &= suffixOk;
                }
              }

              // Use the option
              if (ok) {
                String newText =
                    alias.substring(0, prefixLength) +
                    initialText.substring(newPrefixLength, initialText.length() - newSuffixLength) +
                    alias.substring(alias.length() - suffixLength);
                if (mw.isPossibleAlias(newText)) {
                  return new AutomaticReplacement(initialText, mwName, newText, true);
                }
              }
            }
          }
        }
      }

      return null;
    }
  }

  /** List of automatic replacements */
  private static AutomaticReplacement[] automaticReplacements = {
    // Non existing options
    new AutomaticReplacement("nothumb", null, null, true),

    // IMG_BORDER
    new AutomaticReplacement("rand", MagicWord.IMG_BORDER, "border", false), // de

    // IMG_LEFT
    new AutomaticReplacement("leftt", MagicWord.IMG_LEFT, "left", true),

    // IMG_RIGHT
    new AutomaticReplacement("richt",  MagicWord.IMG_RIGHT, "right", true),
    new AutomaticReplacement("righ",   MagicWord.IMG_RIGHT, "right", true),
    new AutomaticReplacement("rightt", MagicWord.IMG_RIGHT, "right", true),
    new AutomaticReplacement("rigt",   MagicWord.IMG_RIGHT, "right", true),
    new AutomaticReplacement("rigth",  MagicWord.IMG_RIGHT, "right", true),
    new AutomaticReplacement("rigtht", MagicWord.IMG_RIGHT, "right", true),

    // IMG_THUMBNAIL
    new AutomaticReplacement("mini",     MagicWord.IMG_THUMBNAIL, "thumb", true), // de
    new AutomaticReplacement("miniatur", MagicWord.IMG_THUMBNAIL, "thumb", true), // de
    new AutomaticReplacement("thum",     MagicWord.IMG_THUMBNAIL, "thumb", true),
    new AutomaticReplacement("thump",    MagicWord.IMG_THUMBNAIL, "thumb", true),
    new AutomaticReplacement("tuhmb",    MagicWord.IMG_THUMBNAIL, "thumb", true),
    new AutomaticReplacement("tumb",     MagicWord.IMG_THUMBNAIL, "thumb", true),

    // IMG_UPRIGHT
    new AutomaticReplacement("hochkant", MagicWord.IMG_UPRIGHT, "upright", true), // de
    new AutomaticReplacement("uprighht", MagicWord.IMG_UPRIGHT, "upright", true),
    new AutomaticReplacement("uprigt",   MagicWord.IMG_UPRIGHT, "upright", true),
    new AutomaticReplacement("uprigth",  MagicWord.IMG_UPRIGHT, "upright", true),
  };

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
    List<PageElementImage> images = analysis.getImages();
    String contents = analysis.getContents();
    boolean result = false;
    ArrayList<Parameter> params = new ArrayList<>();
    ArrayList<Parameter> paramsFormat = new ArrayList<>();
    ArrayList<Parameter> paramsHAlign = new ArrayList<>();
    ArrayList<Parameter> paramsVAlign = new ArrayList<>();
    for (PageElementImage image : images) {

      // Analyze all parameters of the image
      params.clear();
      paramsFormat.clear();
      paramsHAlign.clear();
      paramsVAlign.clear();
      Collection<Parameter> imageParameters = image.getParameters();
      if (imageParameters != null) {
        for (Parameter param : imageParameters) {
          if (param != null) {
            MagicWord mw = param.getMagicWord();
            if (mw == null) {
              // NO magic word: description or unknown
              params.add(param);
            } else {
              String mwName = mw.getName();
              // Format option: one of border and/or frameless, frame, thumb (or thumbnail)
              if (MagicWord.IMG_BORDER.equals(mwName)) {
                if (paramsFormat.isEmpty() ||
                    !MagicWord.IMG_FRAMELESS.equals(paramsFormat.get(0).getMagicWord().getName())) {
                  paramsFormat.add(param);
                }
              } else
              if (MagicWord.IMG_FRAMELESS.equals(mwName)) {
                if (paramsFormat.isEmpty() ||
                    !MagicWord.IMG_BORDER.equals(paramsFormat.get(0).getMagicWord().getName())) {
                  paramsFormat.add(param);
                }
              } else
              if (MagicWord.IMG_FRAMED.equals(mwName) ||
                  MagicWord.IMG_THUMBNAIL.equals(mwName)) {
                paramsFormat.add(param);
              } else
              // Horizontal alignment option: one of left, right, center, none
              if (MagicWord.IMG_CENTER.equals(mwName) ||
                  MagicWord.IMG_LEFT.equals(mwName) ||
                  MagicWord.IMG_NONE.equals(mwName) ||
                  MagicWord.IMG_RIGHT.equals(mwName)) {
                paramsHAlign.add(param);
              } else
              // Vertical alignment option: one of baseline, sub, super, top, text-top, middle, bottom, text-bottom
              if (MagicWord.IMG_BASELINE.equals(mwName) ||
                  MagicWord.IMG_BOTTOM.equals(mwName) ||
                  MagicWord.IMG_MIDDLE.equals(mwName) ||
                  MagicWord.IMG_SUB.equals(mwName) ||
                  MagicWord.IMG_SUPER.equals(mwName) ||
                  MagicWord.IMG_TEXT_BOTTOM.equals(mwName) ||
                  MagicWord.IMG_TEXT_TOP.equals(mwName) ||
                  MagicWord.IMG_TOP.equals(mwName)) {
                paramsVAlign.add(param);
              }
            }
          }
        }
      }

      // Report images with several parameters that can't be related to a magic word
      if (params.size() > 1) {
        result = true;
        if (errors == null) {
          return result;
        }
        for (int numParam = 0; numParam < params.size(); numParam++) {
          Parameter param = params.get(numParam);
          int beginIndex = image.getBeginIndex() + param.getBeginOffset();
          int endIndex = image.getBeginIndex() + param.getEndOffset();
          if (numParam == params.size() - 1) {
            CheckErrorResult errorResult = createCheckErrorResult(
                analysis, beginIndex, endIndex, ErrorLevel.CORRECT);
            errors.add(errorResult);
          } else {
            boolean hasContents = false;
            for (int index = beginIndex; index < endIndex; index++) {
              if (contents.charAt(index) != ' ') {
                hasContents = true;
              }
            }
            CheckErrorResult errorResult = createCheckErrorResult(
                analysis, beginIndex - 1, endIndex);
            if (!hasContents) {
              errorResult.addReplacement("", true);
            } else {
              AutomaticReplacement replacement = AutomaticReplacement.suggestReplacement(
                  automaticReplacements, analysis.getWikiConfiguration(),
                  param, imageParameters);
              if (replacement != null) {
                String text = replacement.targetText;
                if ((text != null) && !text.isEmpty()) {
                  errorResult.addReplacement("|" + replacement.targetText, replacement.automatic);
                } else {
                  errorResult.addReplacement("", replacement.automatic);
                }
              }
            }
            errors.add(errorResult);
          }
        }
      }

      // Report multiple options for several group of options
      result |= reportMultipleParameters(analysis, errors, image, paramsFormat);
      result |= reportMultipleParameters(analysis, errors, image, paramsHAlign);
      result |= reportMultipleParameters(analysis, errors, image, paramsVAlign);
    }

    return result;
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
      PageElementImage image, ArrayList<Parameter> params) {
    if ((params == null) || (params.size() < 2)) {
      return false;
    }
    if (errors == null) {
      return true;
    }
    for (int numParam = 1; numParam < params.size(); numParam++) {
      Parameter param = params.get(numParam);
      int beginIndex = image.getBeginIndex() + param.getBeginOffset();
      int endIndex = image.getBeginIndex() + param.getEndOffset();
      CheckErrorResult errorResult = createCheckErrorResult(
          analysis, beginIndex - 1, endIndex);
      errorResult.addReplacement("", true);
      errors.add(errorResult);
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
}
