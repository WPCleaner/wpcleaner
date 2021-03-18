/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2021  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm.a5xx.a53x.a534;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.wikipediacleaner.api.configuration.WikiConfiguration;
import org.wikipediacleaner.api.data.PageElementImage.Parameter;
import org.wikipediacleaner.api.data.contents.ContentsUtil;
import org.wikipediacleaner.api.data.contents.magicword.ImageMagicWordType;
import org.wikipediacleaner.api.data.contents.magicword.MagicWord;
import org.wikipediacleaner.api.data.contents.magicword.MagicWordType;

/** Bean for holding automatic replacements */
class AutomaticReplacement {

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
   * @param config Wiki configuration.
   * @param param Current parameter.
   * @param params All parameters.
   * @return Replacement for the initial text.
   */
  public static AutomaticReplacement suggestReplacement(
      WikiConfiguration config,
      Parameter param, Collection<Parameter> params) {

    // Check parameters
    if ((config == null) ||
        (param == null) ||
        (param.getContents() == null)) {
      return null;
    }
    String initialText = param.getContents().trim();

    // Find a suggestion
    AutomaticReplacement replacement = findSuggestion(config, initialText);

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
          if (ImageMagicWordType.FORMAT_OPTIONS.contains(mwType)) {
            if (ImageMagicWordType.FORMAT_OPTIONS.contains(mwTypeTmp)) {
              return new AutomaticReplacement(initialText, null, null, !paramFound);
            }
          } else
          // Horizontal alignment option: one of left, right, center, none
          if (ImageMagicWordType.HORIZONTAL_ALIGN_OPTIONS.contains(mwType)) {
            if (ImageMagicWordType.HORIZONTAL_ALIGN_OPTIONS.contains(mwTypeTmp)) {
              return new AutomaticReplacement(initialText, null, null, !paramFound);
            }
          } else
          // Vertical alignment option: one of baseline, sub, super, top, text-top, middle, bottom, text-bottom
          if (ImageMagicWordType.VERTICAL_ALIGN_OPTIONS.contains(mwType)) {
            if (ImageMagicWordType.VERTICAL_ALIGN_OPTIONS.contains(mwTypeTmp)) {
              return new AutomaticReplacement(initialText, null, null, !paramFound);
            }
          }
        }
      }
    }

    return replacement;
  }

  private static final Set<String> WIDTH_SUFFIXES = Stream
      .of("p", "x", "px", "xp", "dpx", "ppx", "xpx")
      .collect(Collectors.toCollection(HashSet::new));

  /**
   * Find a suggestion for replacement.
   * 
   * @param config Wiki configuration.
   * @param initialText Initial text.
   * @return Replacement for the initial text.
   */
  private static AutomaticReplacement findSuggestion(
      WikiConfiguration config,
      String initialText) {

    // Find a direct suggestion
    for (AutomaticReplacement replacement : AutomaticReplacementFactory.LIST) {
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
    for (AutomaticReplacement replacement : AutomaticReplacementFactory.LIST) {
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

    // Special handling when starting with digits
    int countNumeric = ContentsUtil.moveIndexForwardWhileFound(initialText, 0, "0123456789");
    if (countNumeric >= 1) {
      String suffix = initialText.substring(countNumeric).trim();
      if (WIDTH_SUFFIXES.contains(suffix)) {
        MagicWord magicWord = config.getMagicWordByType(ImageMagicWordType.IMG_WIDTH);
        if ((magicWord != null) && (magicWord.getAliases() != null)) {
          for (String alias : magicWord.getAliases()) {
            int variablePos = alias.indexOf("$1");
            if (variablePos >= 0) {
              String newText =
                  alias.substring(0, variablePos) +
                  initialText.substring(0, countNumeric) +
                  alias.substring(variablePos + 2);
              boolean automatic = (countNumeric > 2) && (countNumeric < 4);
              automatic |= (countNumeric == 2) && (suffix.length() >= 2);
              return new AutomaticReplacement(
                  initialText, ImageMagicWordType.IMG_WIDTH, newText,
                  automatic);
            }
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
                        currentSuffix.equalsIgnoreCase("xp") ||
                        currentSuffix.equalsIgnoreCase("ppx")) {
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