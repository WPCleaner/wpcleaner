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

import org.apache.commons.lang3.StringUtils;
import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.configuration.WikiConfiguration;
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

  /** Bean for holding automatic replacements */
  public static class AutomaticReplacement {

    /** Initial text */
    public final String initialText;

    /** Target magic word */
    public final MagicWordType targetMagicWord;

    /** Target text */
    public final String targetText;

    /** True if replacement can be done automatically */
    public final boolean automatic;

    /** List of magic words with potential options */
    private final static MagicWordType[] mwOptions = {
      ImageMagicWordType.IMG_ALT,
      ImageMagicWordType.IMG_BASELINE,
      ImageMagicWordType.IMG_BORDER,
      ImageMagicWordType.IMG_BOTTOM,
      ImageMagicWordType.IMG_CENTER,
      ImageMagicWordType.IMG_CLASS,
      ImageMagicWordType.IMG_FRAMED,
      ImageMagicWordType.IMG_FRAMELESS,
      ImageMagicWordType.IMG_LANG,
      ImageMagicWordType.IMG_LEFT,
      ImageMagicWordType.IMG_LINK,
      ImageMagicWordType.IMG_LOSSY,
      ImageMagicWordType.IMG_MANUAL_THUMB,
      ImageMagicWordType.IMG_MIDDLE,
      ImageMagicWordType.IMG_NONE,
      ImageMagicWordType.IMG_PAGE,
      ImageMagicWordType.IMG_RIGHT,
      ImageMagicWordType.IMG_SUB,
      ImageMagicWordType.IMG_SUPER,
      ImageMagicWordType.IMG_TEXT_BOTTOM,
      ImageMagicWordType.IMG_TEXT_TOP,
      ImageMagicWordType.IMG_THUMBNAIL,
      ImageMagicWordType.IMG_TOP,
      ImageMagicWordType.IMG_UPRIGHT,
      ImageMagicWordType.IMG_WIDTH,
    };

    /**
     * Extra characters that can be added to the parameter
     */
    private static String EXTRA_CHARACTERS = "abcdefghijklmnopqrstuvwxyz=";

    /**
     * @param initialText Initial text.
     * @param targetMagicWord Target magic word.
     * @param targetText Target text.
     * @param automatic True if replacement can be done automatically.
     */
    public AutomaticReplacement(
        String initialText,
        MagicWordType targetMagicWord, String targetText,
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
              ImageMagicWordType.IMG_ALT.equals(paramTmp.getMagicWord().getType())) {
            hasAltDescription = true;
          }
        }
        if (secondDescription != null) {
          if (secondDescription.equals(initialText)) {
            return new AutomaticReplacement(initialText, null, null, true);
          }
          if (!hasAltDescription) {
            String targetText = "alt=" + initialText;
            MagicWord alt = config.getMagicWordByType(ImageMagicWordType.IMG_ALT);
            if ((alt != null) && (alt.isPossibleAlias(targetText))) {
              return new AutomaticReplacement(
                  initialText, ImageMagicWordType.IMG_ALT, targetText, false);
            }
          }
        }
      }
      if (replacement == null) {
        return null;
      }

      // Check that the suggestion can be applied
      if (replacement.targetMagicWord != null) {
        MagicWordType mwType = replacement.targetMagicWord;
        boolean paramFound = false;
        for (Parameter paramTmp : params) {
          if (paramTmp == param) {
            paramFound = true;
          } else if (paramTmp.getMagicWord() != null) {
            // If option already exists, remove the faulty one
            MagicWordType mwTypeTmp = paramTmp.getMagicWord().getType();
            if (mwType.equals(mwTypeTmp)) {
              return new AutomaticReplacement(initialText, null, null, true);
            }

            // Format option: one of border and/or frameless, frame, thumb (or thumbnail)
            if (ImageMagicWordType.IMG_BORDER.equals(mwType) ||
                ImageMagicWordType.IMG_FRAMELESS.equals(mwType) ||
                ImageMagicWordType.IMG_FRAMED.equals(mwType) ||
                ImageMagicWordType.IMG_THUMBNAIL.equals(mwType)) {
              if (ImageMagicWordType.IMG_BORDER.equals(mwTypeTmp) ||
                  ImageMagicWordType.IMG_FRAMELESS.equals(mwTypeTmp) ||
                  ImageMagicWordType.IMG_FRAMED.equals(mwTypeTmp) ||
                  ImageMagicWordType.IMG_THUMBNAIL.equals(mwTypeTmp)) {
                return new AutomaticReplacement(initialText, null, null, !paramFound);
              }
            } else
            // Horizontal alignment option: one of left, right, center, none
            if (ImageMagicWordType.IMG_CENTER.equals(mwType) ||
                ImageMagicWordType.IMG_LEFT.equals(mwType) ||
                ImageMagicWordType.IMG_NONE.equals(mwType) ||
                ImageMagicWordType.IMG_RIGHT.equals(mwType)) {
              if (ImageMagicWordType.IMG_CENTER.equals(mwTypeTmp) ||
                  ImageMagicWordType.IMG_LEFT.equals(mwTypeTmp) ||
                  ImageMagicWordType.IMG_NONE.equals(mwTypeTmp) ||
                  ImageMagicWordType.IMG_RIGHT.equals(mwTypeTmp)) {
                return new AutomaticReplacement(initialText, null, null, !paramFound);
              }
            } else
            // Vertical alignment option: one of baseline, sub, super, top, text-top, middle, bottom, text-bottom
            if (ImageMagicWordType.IMG_BASELINE.equals(mwType) ||
                ImageMagicWordType.IMG_BOTTOM.equals(mwType) ||
                ImageMagicWordType.IMG_MIDDLE.equals(mwType) ||
                ImageMagicWordType.IMG_SUB.equals(mwType) ||
                ImageMagicWordType.IMG_SUPER.equals(mwType) ||
                ImageMagicWordType.IMG_TEXT_BOTTOM.equals(mwType) ||
                ImageMagicWordType.IMG_TEXT_TOP.equals(mwType) ||
                ImageMagicWordType.IMG_TOP.equals(mwType)) {
              if (ImageMagicWordType.IMG_BASELINE.equals(mwTypeTmp) ||
                  ImageMagicWordType.IMG_BOTTOM.equals(mwTypeTmp) ||
                  ImageMagicWordType.IMG_MIDDLE.equals(mwTypeTmp) ||
                  ImageMagicWordType.IMG_SUB.equals(mwTypeTmp) ||
                  ImageMagicWordType.IMG_SUPER.equals(mwTypeTmp) ||
                  ImageMagicWordType.IMG_TEXT_BOTTOM.equals(mwTypeTmp) ||
                  ImageMagicWordType.IMG_TEXT_TOP.equals(mwTypeTmp) ||
                  ImageMagicWordType.IMG_TOP.equals(mwTypeTmp)) {
                return new AutomaticReplacement(initialText, null, null, !paramFound);
              }
            }
          }
        }
      }

      return replacement;
    }

    /**
     * Find a suggestion for replacement.
     * 
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
          MagicWord magicWord = config.getMagicWordByType(replacement.targetMagicWord);
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
          MagicWord magicWord = config.getMagicWordByType(replacement.targetMagicWord);
          if ((magicWord != null) && (magicWord.isPossibleAlias(replacement.targetText))) {
            return replacement;
          }
        }
      }

      // Special handling for all digits values
      boolean allNumeric = true;
      for (int pos = 0; (pos < initialText.length()) && allNumeric; pos++) {
        allNumeric &= Character.isDigit(initialText.charAt(pos));
      }
      if (allNumeric) {
        MagicWord magicWord = config.getMagicWordByType(ImageMagicWordType.IMG_WIDTH);
        if ((magicWord != null) && (magicWord.getAliases() != null)) {
          for (String alias : magicWord.getAliases()) {
            int variablePos = alias.indexOf("$1");
            if (variablePos >= 0) {
              String newText = alias.substring(0, variablePos) + initialText + alias.substring(variablePos + 2);
              return new AutomaticReplacement(
                  initialText, ImageMagicWordType.IMG_WIDTH, newText,
                  (initialText.length() > 2) && (initialText.length() < 4));
            }
          }
        }
      }

      // Find other suggestions
      AutomaticReplacement possible = findOtherSuggestion(config, initialText, true, true);
      if (possible != null) {
        return possible;
      }
      int variablePos = initialText.indexOf('=');
      if (variablePos < 0) {
        variablePos = initialText.length();
      }
      for (int letterIndex = 0; letterIndex < variablePos; letterIndex++) {
        StringBuilder modified = new StringBuilder(initialText.length() + 1);

        // Try adding a letter
        for (int letter = 0; letter < EXTRA_CHARACTERS.length(); letter++) {
          modified.setLength(0);
          if (letterIndex > 0) {
            modified.append(initialText.substring(0, letterIndex));
          }
          modified.append(EXTRA_CHARACTERS.charAt(letter));
          if (letterIndex < initialText.length()) {
            modified.append(initialText.substring(letterIndex));
          }
          possible = findOtherSuggestion(config, modified.toString(), false, false);
          if (possible != null) {
            return possible;
          }
        }

        // Try replacing a letter
        for (int letter = 0; letter < EXTRA_CHARACTERS.length(); letter++) {
          modified.setLength(0);
          if (letterIndex > 0) {
            modified.append(initialText.substring(0, letterIndex));
          }
          modified.append(EXTRA_CHARACTERS.charAt(letter));
          if (letterIndex < initialText.length()) {
            modified.append(initialText.substring(letterIndex + 1));
          }
          possible = findOtherSuggestion(config, modified.toString(), false, false);
          if (possible != null) {
            return possible;
          }
        }

        // Try removing a letter
        if (variablePos > 1) {
          modified.setLength(0);
          if (letterIndex > 0) {
            modified.append(initialText.substring(0, letterIndex));
          }
          if (letterIndex + 1 < initialText.length()) {
            modified.append(initialText.substring(letterIndex + 1));
          }
          possible = findOtherSuggestion(config, modified.toString(), false, false);
          if (possible != null) {
            return possible;
          }
        }
      }

      return null;
    }

    /**
     * Find an other suggestion for replacement.
     * 
     * @param config Wiki configuration.
     * @param initialText Initial text.
     * @param automatic True if replacement can be automatic.
     * @param complex True if complex replacements can be suggested.
     * @return Replacement for the initial text.
     */
    private static AutomaticReplacement findOtherSuggestion(
        WikiConfiguration config,
        String initialText,
        boolean automatic,
        boolean complex) {
      for (MagicWordType mwType : mwOptions) {
        MagicWord mw = config.getMagicWordByType(mwType);
        if ((mw != null) && (mw.getAliases() != null)) {
          for (String alias : mw.getAliases()) {
            int variablePos = alias.indexOf("$1");
            if (variablePos < 0) {
              if (initialText.equalsIgnoreCase(alias)) {
                return new AutomaticReplacement(initialText, mwType, alias, automatic);
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
                  } else if (aliasSuffix.equals("px") && complex) {
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

              // Check special cases
              if (ok && ImageMagicWordType.IMG_WIDTH.equals(mw.getType())) {
                for (int index = newPrefixLength; (index < initialText.length() - newSuffixLength) && ok; index++) {
                  if (!Character.isDigit(initialText.charAt(index))) {
                    ok = false;
                  }
                }
              }

              // Use the option
              if (ok) {
                String newText =
                    alias.substring(0, prefixLength) +
                    initialText.substring(newPrefixLength, initialText.length() - newSuffixLength) +
                    alias.substring(alias.length() - suffixLength);
                if (mw.isPossibleAlias(newText)) {
                  return new AutomaticReplacement(initialText, mwType, newText, automatic);
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
    new AutomaticReplacement("align",      null, null, true),
    new AutomaticReplacement("alt",        null, null, true),
    new AutomaticReplacement("caption",    null, null, true),
    new AutomaticReplacement("default",    null, null, true),
    new AutomaticReplacement("file",       null, null, true),
    new AutomaticReplacement("horizontal", null, null, true),
    new AutomaticReplacement("landscape",  null, null, true),
    new AutomaticReplacement("links",      null, null, true),
    new AutomaticReplacement("logo",       null, null, true),
    new AutomaticReplacement("maxi",       null, null, true),
    new AutomaticReplacement("nothumb",    null, null, true),
    new AutomaticReplacement("panorama",   null, null, true),
    new AutomaticReplacement("portrait",   null, null, true),
    new AutomaticReplacement("small",      null, null, true),
    new AutomaticReplacement("text",       null, null, true),
    new AutomaticReplacement("title",      null, null, true),
    new AutomaticReplacement("vertical",   null, null, true),
    new AutomaticReplacement("view",       null, null, true),
    new AutomaticReplacement("wide",       null, null, true),

    // IMG_BORDER
    new AutomaticReplacement("rand", ImageMagicWordType.IMG_BORDER, "border", false), // de

    // IMG_CENTER
    new AutomaticReplacement("align=center", ImageMagicWordType.IMG_CENTER, "center", true),
    new AutomaticReplacement("align:center", ImageMagicWordType.IMG_CENTER, "center", true),
    new AutomaticReplacement("centro",       ImageMagicWordType.IMG_CENTER, "center", true),

    // IMG_FRAMELESS
    new AutomaticReplacement("rahmenlos", ImageMagicWordType.IMG_FRAMELESS, "frameless", true),
    new AutomaticReplacement("безрамки",  ImageMagicWordType.IMG_FRAMELESS, "frameless", true),

    // IMG_LEFT
    new AutomaticReplacement("align=left", ImageMagicWordType.IMG_LEFT, "left", true),
    new AutomaticReplacement("align:left", ImageMagicWordType.IMG_LEFT, "left", true),
    new AutomaticReplacement("esquerda",   ImageMagicWordType.IMG_LEFT, "left", true),
    new AutomaticReplacement("esquerra",   ImageMagicWordType.IMG_LEFT, "left", true),
    new AutomaticReplacement("gauche",     ImageMagicWordType.IMG_LEFT, "left", true),
    new AutomaticReplacement("izquierda",  ImageMagicWordType.IMG_LEFT, "left", true),
    new AutomaticReplacement("leftt",      ImageMagicWordType.IMG_LEFT, "left", true),
    new AutomaticReplacement("ліворуч",    ImageMagicWordType.IMG_LEFT, "left", true),
    new AutomaticReplacement("שמאל",       ImageMagicWordType.IMG_LEFT, "left", true),

    // IMG_RIGHT
    new AutomaticReplacement("align=right", ImageMagicWordType.IMG_RIGHT, "right", true),
    new AutomaticReplacement("align:right", ImageMagicWordType.IMG_RIGHT, "right", true),
    new AutomaticReplacement("derecha",     ImageMagicWordType.IMG_RIGHT, "right", true),
    new AutomaticReplacement("desno",       ImageMagicWordType.IMG_RIGHT, "right", true),
    new AutomaticReplacement("destra",      ImageMagicWordType.IMG_RIGHT, "right", true),
    new AutomaticReplacement("direita",     ImageMagicWordType.IMG_RIGHT, "right", true),
    new AutomaticReplacement("dreta",       ImageMagicWordType.IMG_RIGHT, "right", true),
    new AutomaticReplacement("float right", ImageMagicWordType.IMG_RIGHT, "right", true),
    new AutomaticReplacement("float=right", ImageMagicWordType.IMG_RIGHT, "right", true),
    new AutomaticReplacement("float:right", ImageMagicWordType.IMG_RIGHT, "right", true),
    new AutomaticReplacement("floatright",  ImageMagicWordType.IMG_RIGHT, "right", true),
    new AutomaticReplacement("ight",        ImageMagicWordType.IMG_RIGHT, "right", true),
    new AutomaticReplacement("rechts",      ImageMagicWordType.IMG_RIGHT, "right", true),
    new AutomaticReplacement("reght",       ImageMagicWordType.IMG_RIGHT, "right", true),
    new AutomaticReplacement("rght",        ImageMagicWordType.IMG_RIGHT, "right", true),
    new AutomaticReplacement("ribght",      ImageMagicWordType.IMG_RIGHT, "right", true),
    new AutomaticReplacement("richt",       ImageMagicWordType.IMG_RIGHT, "right", true),
    new AutomaticReplacement("righ",        ImageMagicWordType.IMG_RIGHT, "right", true),
    new AutomaticReplacement("righjt",      ImageMagicWordType.IMG_RIGHT, "right", true),
    new AutomaticReplacement("righr",       ImageMagicWordType.IMG_RIGHT, "right", true),
    new AutomaticReplacement("righte",      ImageMagicWordType.IMG_RIGHT, "right", true),
    new AutomaticReplacement("rightg",      ImageMagicWordType.IMG_RIGHT, "right", true),
    new AutomaticReplacement("rightl",      ImageMagicWordType.IMG_RIGHT, "right", true),
    new AutomaticReplacement("rightt",      ImageMagicWordType.IMG_RIGHT, "right", true),
    new AutomaticReplacement("rightx",      ImageMagicWordType.IMG_RIGHT, "right", true),
    new AutomaticReplacement("righty",      ImageMagicWordType.IMG_RIGHT, "right", true),
    new AutomaticReplacement("right1",      ImageMagicWordType.IMG_RIGHT, "right", true),
    new AutomaticReplacement("right2",      ImageMagicWordType.IMG_RIGHT, "right", true),
    new AutomaticReplacement("righy",       ImageMagicWordType.IMG_RIGHT, "right", true),
    new AutomaticReplacement("righyt",      ImageMagicWordType.IMG_RIGHT, "right", true),
    new AutomaticReplacement("rigjt",       ImageMagicWordType.IMG_RIGHT, "right", true),
    new AutomaticReplacement("rignt",       ImageMagicWordType.IMG_RIGHT, "right", true),
    new AutomaticReplacement("rigt",        ImageMagicWordType.IMG_RIGHT, "right", true),
    new AutomaticReplacement("rigth",       ImageMagicWordType.IMG_RIGHT, "right", true),
    new AutomaticReplacement("rigtht",      ImageMagicWordType.IMG_RIGHT, "right", true),
    new AutomaticReplacement("rihgt",       ImageMagicWordType.IMG_RIGHT, "right", true),
    new AutomaticReplacement("roght",       ImageMagicWordType.IMG_RIGHT, "right", true),
    new AutomaticReplacement("rught",       ImageMagicWordType.IMG_RIGHT, "right", true),
    new AutomaticReplacement("праворуч",    ImageMagicWordType.IMG_RIGHT, "right", true),
    new AutomaticReplacement("дясно",       ImageMagicWordType.IMG_RIGHT, "right", true),
    new AutomaticReplacement("справа",      ImageMagicWordType.IMG_RIGHT, "right", true),

    // IMG_THUMBNAIL
    new AutomaticReplacement("mini",              ImageMagicWordType.IMG_THUMBNAIL, "thumb", true), // de
    new AutomaticReplacement("miniatur",          ImageMagicWordType.IMG_THUMBNAIL, "thumb", true), // de
    new AutomaticReplacement("miniatura",         ImageMagicWordType.IMG_THUMBNAIL, "thumb", true),
    new AutomaticReplacement("miniaturadeimagen", ImageMagicWordType.IMG_THUMBNAIL, "thumb", true),
    new AutomaticReplacement("miniature",         ImageMagicWordType.IMG_THUMBNAIL, "thumb", true),
    new AutomaticReplacement("miniatyr",          ImageMagicWordType.IMG_THUMBNAIL, "thumb", true),
    new AutomaticReplacement("thum",              ImageMagicWordType.IMG_THUMBNAIL, "thumb", true),
    new AutomaticReplacement("thump",             ImageMagicWordType.IMG_THUMBNAIL, "thumb", true),
    new AutomaticReplacement("tuhmb",             ImageMagicWordType.IMG_THUMBNAIL, "thumb", true),
    new AutomaticReplacement("tumb",              ImageMagicWordType.IMG_THUMBNAIL, "thumb", true),
    new AutomaticReplacement("мини",              ImageMagicWordType.IMG_THUMBNAIL, "thumb", true),
    new AutomaticReplacement("ממוזער",              ImageMagicWordType.IMG_THUMBNAIL, "thumb", true),

    // IMG_UPRIGHT
    new AutomaticReplacement("align=upright", ImageMagicWordType.IMG_UPRIGHT, "upright", true),
    new AutomaticReplacement("hochkant",      ImageMagicWordType.IMG_UPRIGHT, "upright", true), // de
    new AutomaticReplacement("uoright",       ImageMagicWordType.IMG_UPRIGHT, "upright", true),
    new AutomaticReplacement("upleft",        ImageMagicWordType.IMG_UPRIGHT, "upright", true),
    new AutomaticReplacement("uprighht",      ImageMagicWordType.IMG_UPRIGHT, "upright", true),
    new AutomaticReplacement("uprigt",        ImageMagicWordType.IMG_UPRIGHT, "upright", true),
    new AutomaticReplacement("uprigth",       ImageMagicWordType.IMG_UPRIGHT, "upright", true),
    new AutomaticReplacement("uptight",       ImageMagicWordType.IMG_UPRIGHT, "upright", true),
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
    HashMap<MagicWordType, ArrayList<Parameter>> paramsOther = new HashMap<>();
    for (PageElementImage image : images) {

      // Analyze all parameters of the image
      params.clear();
      paramsFormat.clear();
      paramsHAlign.clear();
      paramsVAlign.clear();
      paramsOther.clear();
      Collection<Parameter> imageParameters = image.getParameters();
      if (imageParameters != null) {
        for (Parameter param : imageParameters) {
          if (param != null) {
            MagicWord mw = param.getMagicWord();
            if (mw == null) {
              // NO magic word: description or unknown
              params.add(param);
            } else {
              MagicWordType mwType = mw.getType();
              // Format option: one of border and/or frameless, frame, thumb (or thumbnail)
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
              } else
              // Horizontal alignment option: one of left, right, center, none
              if (ImageMagicWordType.IMG_CENTER.equals(mwType) ||
                  ImageMagicWordType.IMG_LEFT.equals(mwType) ||
                  ImageMagicWordType.IMG_NONE.equals(mwType) ||
                  ImageMagicWordType.IMG_RIGHT.equals(mwType)) {
                paramsHAlign.add(param);
              } else
              // Vertical alignment option: one of baseline, sub, super, top, text-top, middle, bottom, text-bottom
              if (ImageMagicWordType.IMG_BASELINE.equals(mwType) ||
                  ImageMagicWordType.IMG_BOTTOM.equals(mwType) ||
                  ImageMagicWordType.IMG_MIDDLE.equals(mwType) ||
                  ImageMagicWordType.IMG_SUB.equals(mwType) ||
                  ImageMagicWordType.IMG_SUPER.equals(mwType) ||
                  ImageMagicWordType.IMG_TEXT_BOTTOM.equals(mwType) ||
                  ImageMagicWordType.IMG_TEXT_TOP.equals(mwType) ||
                  ImageMagicWordType.IMG_TOP.equals(mwType)) {
                paramsVAlign.add(param);
              } else {
                ArrayList<Parameter> tmpList = paramsOther.get(mwType);
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

      // Report images with several parameters that can't be related to a magic word
      if (params.size() > 1) {
        result = true;
        if (errors == null) {
          return result;
        }
        boolean reported = false;

        // Analyze if there's a risk of error
        boolean safe = true;
        boolean safeEmpty = true;
        String imageText = contents.substring(image.getBeginIndex(), image.getEndIndex());
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
                safeEmpty = false;
              }
            }
          }
        }

        // Case when last parameter is empty
        if (!reported) {
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
            reported = true;
          }
        }

        // Check when last parameter is not empty
        if (!reported) {
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
              } else {
                AutomaticReplacement replacement = AutomaticReplacement.suggestReplacement(
                    automaticReplacements, analysis.getWikiConfiguration(),
                    param, imageParameters);
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
          }
        }
      }

      // Report multiple options for several group of options
      result |= reportMultipleParameters(analysis, errors, image, paramsFormat, false);
      result |= reportMultipleParameters(analysis, errors, image, paramsHAlign, false);
      result |= reportMultipleParameters(analysis, errors, image, paramsVAlign, false);
      for (ArrayList<Parameter> paramOther : paramsOther.values()) {
        result |= reportMultipleParameters(analysis, errors, image, paramOther, params.isEmpty());
      }
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
      PageElementImage image, ArrayList<Parameter> params,
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
