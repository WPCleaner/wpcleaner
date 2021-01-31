/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2019  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.utils.string;

import java.util.HashMap;
import java.util.Map;

/**
 * Utility class for characters.
 */
public class CharacterUtils {

  /** Map of character conversion to upper case to override default behavior. */
  private final static Map<Character, Character> UPPERCASE_CONVERSION = new HashMap<>();

  /** Lower boundary of the override map */
  private final static int LOWER_BOUNDARY;

  /** Upper boundary of the override map */
  private final static int UPPER_BOUNDARY;

  /**
   * Put the first letter to upper case following MediaWiki rules.
   * 
   * @param text Original text.
   * @return Modified text with first letter to upper case.
   */
  public static String ucFirst(String text) {
    if ((text == null) || (text.isEmpty())) {
      return text;
    }
    char firstChar = text.charAt(0);
    if (Character.isUpperCase(firstChar) || !Character.isLowerCase(firstChar)) {
      return text;
    }
    if ((firstChar >= LOWER_BOUNDARY) && (firstChar <= UPPER_BOUNDARY)) {
      Character uppercase = UPPERCASE_CONVERSION.get(firstChar);
      if (uppercase != null) {
        firstChar = uppercase.charValue();
      } else {
        firstChar = Character.toUpperCase(firstChar);
      }
    } else {
      firstChar = Character.toUpperCase(firstChar);
    }
    return "" + firstChar + text.substring(1);
  }

  /**
   * Compare two characters ignoring case.
   * 
   * @param first First character.
   * @param second Second character.
   * @return True if the two characters are equal ignoring case.
   */
  public static boolean equalsIgnoreCase(char first, char second) {
    return Character.toUpperCase(first) == Character.toUpperCase(second);
  }

  /** Whitespace characters */
  public final static String WHITESPACE = " \u00A0";

  /** En dash character */
  public static char EN_DASH = '\u2013';

  /** Em dash character */
  public static char EM_DASH = '\u2014';

  /** Dash characters */
  public static final String DASHES = "-" + EN_DASH + EM_DASH;

  /** Punctuation characters */
  public static final String PUNCTUATION = ",.;:!?()\"" + DASHES;

  /**
   * @param character Character to be tested.
   * @return True if the character should be considered as a whitespace.
   */
  public static boolean isWhitespace(char character) {
    return (WHITESPACE.indexOf(character) >= 0);
  }

  /**
   * @param character Character to be tested.
   * @return True if the character should be considered as a punctuation.
   */
  public static boolean isPunctuation(char character) {
    return (PUNCTUATION.indexOf(character) >= 0);
  }

  /**
   * @param character Character to be tested.
   * @return True if the character should be considered as a dash.
   */
  public static boolean isDash(char character) {
    return (DASHES.indexOf(character) >= 0);
  }

  /**
   * @param character Character to be tested.
   * @return True if the character should be considered as a classic letter.
   * In comparison, {@link java.lang.Character#isLetter(char) Character.isLetter(char)} is more permissive.
   */
  public static boolean isClassicLetter(char character) {
    if (!Character.isLetter(character)) {
      return false;
    }
    // See various Unicode blocks:
    // * Basic Latin: https://en.wikipedia.org/wiki/Basic_Latin_(Unicode_block)
    // * Latin-1 Supplement: https://en.wikipedia.org/wiki/Latin-1_Supplement_(Unicode_block)
    // * Latin Extended-A: https://en.wikipedia.org/wiki/Latin_Extended-A
    // * Latin Extended-B: https://en.wikipedia.org/wiki/Latin_Extended-B
    // Others?
    if (((character >= 'A') && (character <= 'Z')) ||
        ((character >= 'a') && (character <= 'z')) ||
        ((character >= 'À') && (character <= 'Ö')) ||
        ((character >= 'Ø') && (character <= 'ö')) ||
        ((character >= 'ø') && (character <= 'ÿ')) ||
        ((character >= 'Ā') && (character <= 'ň')) ||
        ((character >= 'Ŋ') && (character <= 'ſ')) ||
        ((character >= 'ƀ') && (character <= 'ɏ'))) {
      return true;
    }
    return false;
  }

  /**
   * @param character Character to be tested.
   * @return True if the character should be considered as a classic digit.
   * In comparison, {@link java.lang.Character#isDigit(char) Character.isDigit(char)} is more permissive.
   */
  public static boolean isClassicDigit(char character) {
    if ((character >= '0') && (character <= '9')) {
      return true;
    }
    return false;
  }

  /**
   * @param character Character to be tested.
   * @param text Text to be tested.
   * @return True if the character is found in the text.
   */
  public static boolean isInText(char character, String text) {
    return (text != null) && (text.indexOf(character) >= 0);
  }

  /**
   * Trim text with whitespace characters used by MediaWiki.
   * 
   * @param text Original text.
   * @return Trimmed text.
   */
  public static String trim(String text) {
    if ((text == null) || (text.isEmpty())) {
      return text;
    }
    int beginIndex = 0;
    int endIndex = text.length();
    while ((beginIndex < endIndex) && isWhitespace(text.charAt(beginIndex))) {
      beginIndex++;
    }
    while ((endIndex > beginIndex) && isWhitespace(text.charAt(endIndex - 1))) {
      endIndex--;
    }
    return text.substring(beginIndex, endIndex);
  }

  /**
   * Private constructor.
   */
  private CharacterUtils() {
    // Nothing to do
  }

  static {
    // Initialize map of character conversion to upper case
    // Retrieved from: https://github.com/wikimedia/operations-mediawiki-config/blob/master/wmf-config/Php72ToUpper.php
    UPPERCASE_CONVERSION.put('ƀ', 'ƀ');
    UPPERCASE_CONVERSION.put('ƚ', 'ƚ');
    UPPERCASE_CONVERSION.put('ǅ', 'ǅ');
    UPPERCASE_CONVERSION.put('ǆ', 'ǅ');
    UPPERCASE_CONVERSION.put('ǈ', 'ǈ');
    UPPERCASE_CONVERSION.put('ǉ', 'ǈ');
    UPPERCASE_CONVERSION.put('ǋ', 'ǋ');
    UPPERCASE_CONVERSION.put('ǌ', 'ǋ');
    UPPERCASE_CONVERSION.put('ǲ', 'ǲ');
    UPPERCASE_CONVERSION.put('ǳ', 'ǲ');
    UPPERCASE_CONVERSION.put('ȼ', 'ȼ');
    UPPERCASE_CONVERSION.put('ȿ', 'ȿ');
    UPPERCASE_CONVERSION.put('ɀ', 'ɀ');
    UPPERCASE_CONVERSION.put('ɂ', 'ɂ');
    UPPERCASE_CONVERSION.put('ɇ', 'ɇ');
    UPPERCASE_CONVERSION.put('ɉ', 'ɉ');
    UPPERCASE_CONVERSION.put('ɋ', 'ɋ');
    UPPERCASE_CONVERSION.put('ɍ', 'ɍ');
    UPPERCASE_CONVERSION.put('ɏ', 'ɏ');
    UPPERCASE_CONVERSION.put('ɐ', 'ɐ');
    UPPERCASE_CONVERSION.put('ɑ', 'ɑ');
    UPPERCASE_CONVERSION.put('ɒ', 'ɒ');
    UPPERCASE_CONVERSION.put('ɜ', 'ɜ');
    UPPERCASE_CONVERSION.put('ɡ', 'ɡ');
    UPPERCASE_CONVERSION.put('ɥ', 'ɥ');
    UPPERCASE_CONVERSION.put('ɦ', 'ɦ');
    UPPERCASE_CONVERSION.put('ɪ', 'ɪ');
    UPPERCASE_CONVERSION.put('ɫ', 'ɫ');
    UPPERCASE_CONVERSION.put('ɬ', 'ɬ');
    UPPERCASE_CONVERSION.put('ɱ', 'ɱ');
    UPPERCASE_CONVERSION.put('ɽ', 'ɽ');
    UPPERCASE_CONVERSION.put('ʇ', 'ʇ');
    UPPERCASE_CONVERSION.put('ʉ', 'ʉ');
    UPPERCASE_CONVERSION.put('ʌ', 'ʌ');
    UPPERCASE_CONVERSION.put('ʝ', 'ʝ');
    UPPERCASE_CONVERSION.put('ʞ', 'ʞ');
    UPPERCASE_CONVERSION.put('ͱ', 'ͱ');
    UPPERCASE_CONVERSION.put('ͳ', 'ͳ');
    UPPERCASE_CONVERSION.put('ͷ', 'ͷ');
    UPPERCASE_CONVERSION.put('ͻ', 'ͻ');
    UPPERCASE_CONVERSION.put('ͼ', 'ͼ');
    UPPERCASE_CONVERSION.put('ͽ', 'ͽ');
    UPPERCASE_CONVERSION.put('ϗ', 'ϗ');
    UPPERCASE_CONVERSION.put('ϲ', 'Σ');
    UPPERCASE_CONVERSION.put('ϳ', 'ϳ');
    UPPERCASE_CONVERSION.put('ϸ', 'ϸ');
    UPPERCASE_CONVERSION.put('ϻ', 'ϻ');
    UPPERCASE_CONVERSION.put('ӏ', 'ӏ');
    UPPERCASE_CONVERSION.put('ӷ', 'ӷ');
    UPPERCASE_CONVERSION.put('ӻ', 'ӻ');
    UPPERCASE_CONVERSION.put('ӽ', 'ӽ');
    UPPERCASE_CONVERSION.put('ӿ', 'ӿ');
    UPPERCASE_CONVERSION.put('ԑ', 'ԑ');
    UPPERCASE_CONVERSION.put('ԓ', 'ԓ');
    UPPERCASE_CONVERSION.put('ԕ', 'ԕ');
    UPPERCASE_CONVERSION.put('ԗ', 'ԗ');
    UPPERCASE_CONVERSION.put('ԙ', 'ԙ');
    UPPERCASE_CONVERSION.put('ԛ', 'ԛ');
    UPPERCASE_CONVERSION.put('ԝ', 'ԝ');
    UPPERCASE_CONVERSION.put('ԟ', 'ԟ');
    UPPERCASE_CONVERSION.put('ԡ', 'ԡ');
    UPPERCASE_CONVERSION.put('ԣ', 'ԣ');
    UPPERCASE_CONVERSION.put('ԥ', 'ԥ');
    UPPERCASE_CONVERSION.put('ԧ', 'ԧ');
    UPPERCASE_CONVERSION.put('ԩ', 'ԩ');
    UPPERCASE_CONVERSION.put('ԫ', 'ԫ');
    UPPERCASE_CONVERSION.put('ԭ', 'ԭ');
    UPPERCASE_CONVERSION.put('ԯ', 'ԯ');
    UPPERCASE_CONVERSION.put('ᏸ', 'ᏸ');
    UPPERCASE_CONVERSION.put('ᏹ', 'ᏹ');
    UPPERCASE_CONVERSION.put('ᏺ', 'ᏺ');
    UPPERCASE_CONVERSION.put('ᏻ', 'ᏻ');
    UPPERCASE_CONVERSION.put('ᏼ', 'ᏼ');
    UPPERCASE_CONVERSION.put('ᏽ', 'ᏽ');
    UPPERCASE_CONVERSION.put('ᲀ', 'ᲀ');
    UPPERCASE_CONVERSION.put('ᲁ', 'ᲁ');
    UPPERCASE_CONVERSION.put('ᲂ', 'ᲂ');
    UPPERCASE_CONVERSION.put('ᲃ', 'ᲃ');
    UPPERCASE_CONVERSION.put('ᲄ', 'ᲄ');
    UPPERCASE_CONVERSION.put('ᲅ', 'ᲅ');
    UPPERCASE_CONVERSION.put('ᲆ', 'ᲆ');
    UPPERCASE_CONVERSION.put('ᲇ', 'ᲇ');
    UPPERCASE_CONVERSION.put('ᲈ', 'ᲈ');
    UPPERCASE_CONVERSION.put('ᵹ', 'ᵹ');
    UPPERCASE_CONVERSION.put('ᵽ', 'ᵽ');
    UPPERCASE_CONVERSION.put('ỻ', 'ỻ');
    UPPERCASE_CONVERSION.put('ỽ', 'ỽ');
    UPPERCASE_CONVERSION.put('ỿ', 'ỿ');
    UPPERCASE_CONVERSION.put('ⅎ', 'ⅎ');
    UPPERCASE_CONVERSION.put('ↄ', 'ↄ');
    UPPERCASE_CONVERSION.put('ⰰ', 'ⰰ');
    UPPERCASE_CONVERSION.put('ⰱ', 'ⰱ');
    UPPERCASE_CONVERSION.put('ⰲ', 'ⰲ');
    UPPERCASE_CONVERSION.put('ⰳ', 'ⰳ');
    UPPERCASE_CONVERSION.put('ⰴ', 'ⰴ');
    UPPERCASE_CONVERSION.put('ⰵ', 'ⰵ');
    UPPERCASE_CONVERSION.put('ⰶ', 'ⰶ');
    UPPERCASE_CONVERSION.put('ⰷ', 'ⰷ');
    UPPERCASE_CONVERSION.put('ⰸ', 'ⰸ');
    UPPERCASE_CONVERSION.put('ⰹ', 'ⰹ');
    UPPERCASE_CONVERSION.put('ⰺ', 'ⰺ');
    UPPERCASE_CONVERSION.put('ⰻ', 'ⰻ');
    UPPERCASE_CONVERSION.put('ⰼ', 'ⰼ');
    UPPERCASE_CONVERSION.put('ⰽ', 'ⰽ');
    UPPERCASE_CONVERSION.put('ⰾ', 'ⰾ');
    UPPERCASE_CONVERSION.put('ⰿ', 'ⰿ');
    UPPERCASE_CONVERSION.put('ⱀ', 'ⱀ');
    UPPERCASE_CONVERSION.put('ⱁ', 'ⱁ');
    UPPERCASE_CONVERSION.put('ⱂ', 'ⱂ');
    UPPERCASE_CONVERSION.put('ⱃ', 'ⱃ');
    UPPERCASE_CONVERSION.put('ⱄ', 'ⱄ');
    UPPERCASE_CONVERSION.put('ⱅ', 'ⱅ');
    UPPERCASE_CONVERSION.put('ⱆ', 'ⱆ');
    UPPERCASE_CONVERSION.put('ⱇ', 'ⱇ');
    UPPERCASE_CONVERSION.put('ⱈ', 'ⱈ');
    UPPERCASE_CONVERSION.put('ⱉ', 'ⱉ');
    UPPERCASE_CONVERSION.put('ⱊ', 'ⱊ');
    UPPERCASE_CONVERSION.put('ⱋ', 'ⱋ');
    UPPERCASE_CONVERSION.put('ⱌ', 'ⱌ');
    UPPERCASE_CONVERSION.put('ⱍ', 'ⱍ');
    UPPERCASE_CONVERSION.put('ⱎ', 'ⱎ');
    UPPERCASE_CONVERSION.put('ⱏ', 'ⱏ');
    UPPERCASE_CONVERSION.put('ⱐ', 'ⱐ');
    UPPERCASE_CONVERSION.put('ⱑ', 'ⱑ');
    UPPERCASE_CONVERSION.put('ⱒ', 'ⱒ');
    UPPERCASE_CONVERSION.put('ⱓ', 'ⱓ');
    UPPERCASE_CONVERSION.put('ⱔ', 'ⱔ');
    UPPERCASE_CONVERSION.put('ⱕ', 'ⱕ');
    UPPERCASE_CONVERSION.put('ⱖ', 'ⱖ');
    UPPERCASE_CONVERSION.put('ⱗ', 'ⱗ');
    UPPERCASE_CONVERSION.put('ⱘ', 'ⱘ');
    UPPERCASE_CONVERSION.put('ⱙ', 'ⱙ');
    UPPERCASE_CONVERSION.put('ⱚ', 'ⱚ');
    UPPERCASE_CONVERSION.put('ⱛ', 'ⱛ');
    UPPERCASE_CONVERSION.put('ⱜ', 'ⱜ');
    UPPERCASE_CONVERSION.put('ⱝ', 'ⱝ');
    UPPERCASE_CONVERSION.put('ⱞ', 'ⱞ');
    UPPERCASE_CONVERSION.put('ⱡ', 'ⱡ');
    UPPERCASE_CONVERSION.put('ⱥ', 'ⱥ');
    UPPERCASE_CONVERSION.put('ⱦ', 'ⱦ');
    UPPERCASE_CONVERSION.put('ⱨ', 'ⱨ');
    UPPERCASE_CONVERSION.put('ⱪ', 'ⱪ');
    UPPERCASE_CONVERSION.put('ⱬ', 'ⱬ');
    UPPERCASE_CONVERSION.put('ⱳ', 'ⱳ');
    UPPERCASE_CONVERSION.put('ⱶ', 'ⱶ');
    UPPERCASE_CONVERSION.put('ⲁ', 'ⲁ');
    UPPERCASE_CONVERSION.put('ⲃ', 'ⲃ');
    UPPERCASE_CONVERSION.put('ⲅ', 'ⲅ');
    UPPERCASE_CONVERSION.put('ⲇ', 'ⲇ');
    UPPERCASE_CONVERSION.put('ⲉ', 'ⲉ');
    UPPERCASE_CONVERSION.put('ⲋ', 'ⲋ');
    UPPERCASE_CONVERSION.put('ⲍ', 'ⲍ');
    UPPERCASE_CONVERSION.put('ⲏ', 'ⲏ');
    UPPERCASE_CONVERSION.put('ⲑ', 'ⲑ');
    UPPERCASE_CONVERSION.put('ⲓ', 'ⲓ');
    UPPERCASE_CONVERSION.put('ⲕ', 'ⲕ');
    UPPERCASE_CONVERSION.put('ⲗ', 'ⲗ');
    UPPERCASE_CONVERSION.put('ⲙ', 'ⲙ');
    UPPERCASE_CONVERSION.put('ⲛ', 'ⲛ');
    UPPERCASE_CONVERSION.put('ⲝ', 'ⲝ');
    UPPERCASE_CONVERSION.put('ⲟ', 'ⲟ');
    UPPERCASE_CONVERSION.put('ⲡ', 'ⲡ');
    UPPERCASE_CONVERSION.put('ⲣ', 'ⲣ');
    UPPERCASE_CONVERSION.put('ⲥ', 'ⲥ');
    UPPERCASE_CONVERSION.put('ⲧ', 'ⲧ');
    UPPERCASE_CONVERSION.put('ⲩ', 'ⲩ');
    UPPERCASE_CONVERSION.put('ⲫ', 'ⲫ');
    UPPERCASE_CONVERSION.put('ⲭ', 'ⲭ');
    UPPERCASE_CONVERSION.put('ⲯ', 'ⲯ');
    UPPERCASE_CONVERSION.put('ⲱ', 'ⲱ');
    UPPERCASE_CONVERSION.put('ⲳ', 'ⲳ');
    UPPERCASE_CONVERSION.put('ⲵ', 'ⲵ');
    UPPERCASE_CONVERSION.put('ⲷ', 'ⲷ');
    UPPERCASE_CONVERSION.put('ⲹ', 'ⲹ');
    UPPERCASE_CONVERSION.put('ⲻ', 'ⲻ');
    UPPERCASE_CONVERSION.put('ⲽ', 'ⲽ');
    UPPERCASE_CONVERSION.put('ⲿ', 'ⲿ');
    UPPERCASE_CONVERSION.put('ⳁ', 'ⳁ');
    UPPERCASE_CONVERSION.put('ⳃ', 'ⳃ');
    UPPERCASE_CONVERSION.put('ⳅ', 'ⳅ');
    UPPERCASE_CONVERSION.put('ⳇ', 'ⳇ');
    UPPERCASE_CONVERSION.put('ⳉ', 'ⳉ');
    UPPERCASE_CONVERSION.put('ⳋ', 'ⳋ');
    UPPERCASE_CONVERSION.put('ⳍ', 'ⳍ');
    UPPERCASE_CONVERSION.put('ⳏ', 'ⳏ');
    UPPERCASE_CONVERSION.put('ⳑ', 'ⳑ');
    UPPERCASE_CONVERSION.put('ⳓ', 'ⳓ');
    UPPERCASE_CONVERSION.put('ⳕ', 'ⳕ');
    UPPERCASE_CONVERSION.put('ⳗ', 'ⳗ');
    UPPERCASE_CONVERSION.put('ⳙ', 'ⳙ');
    UPPERCASE_CONVERSION.put('ⳛ', 'ⳛ');
    UPPERCASE_CONVERSION.put('ⳝ', 'ⳝ');
    UPPERCASE_CONVERSION.put('ⳟ', 'ⳟ');
    UPPERCASE_CONVERSION.put('ⳡ', 'ⳡ');
    UPPERCASE_CONVERSION.put('ⳣ', 'ⳣ');
    UPPERCASE_CONVERSION.put('ⳬ', 'ⳬ');
    UPPERCASE_CONVERSION.put('ⳮ', 'ⳮ');
    UPPERCASE_CONVERSION.put('ⳳ', 'ⳳ');
    UPPERCASE_CONVERSION.put('ⴀ', 'ⴀ');
    UPPERCASE_CONVERSION.put('ⴁ', 'ⴁ');
    UPPERCASE_CONVERSION.put('ⴂ', 'ⴂ');
    UPPERCASE_CONVERSION.put('ⴃ', 'ⴃ');
    UPPERCASE_CONVERSION.put('ⴄ', 'ⴄ');
    UPPERCASE_CONVERSION.put('ⴅ', 'ⴅ');
    UPPERCASE_CONVERSION.put('ⴆ', 'ⴆ');
    UPPERCASE_CONVERSION.put('ⴇ', 'ⴇ');
    UPPERCASE_CONVERSION.put('ⴈ', 'ⴈ');
    UPPERCASE_CONVERSION.put('ⴉ', 'ⴉ');
    UPPERCASE_CONVERSION.put('ⴊ', 'ⴊ');
    UPPERCASE_CONVERSION.put('ⴋ', 'ⴋ');
    UPPERCASE_CONVERSION.put('ⴌ', 'ⴌ');
    UPPERCASE_CONVERSION.put('ⴍ', 'ⴍ');
    UPPERCASE_CONVERSION.put('ⴎ', 'ⴎ');
    UPPERCASE_CONVERSION.put('ⴏ', 'ⴏ');
    UPPERCASE_CONVERSION.put('ⴐ', 'ⴐ');
    UPPERCASE_CONVERSION.put('ⴑ', 'ⴑ');
    UPPERCASE_CONVERSION.put('ⴒ', 'ⴒ');
    UPPERCASE_CONVERSION.put('ⴓ', 'ⴓ');
    UPPERCASE_CONVERSION.put('ⴔ', 'ⴔ');
    UPPERCASE_CONVERSION.put('ⴕ', 'ⴕ');
    UPPERCASE_CONVERSION.put('ⴖ', 'ⴖ');
    UPPERCASE_CONVERSION.put('ⴗ', 'ⴗ');
    UPPERCASE_CONVERSION.put('ⴘ', 'ⴘ');
    UPPERCASE_CONVERSION.put('ⴙ', 'ⴙ');
    UPPERCASE_CONVERSION.put('ⴚ', 'ⴚ');
    UPPERCASE_CONVERSION.put('ⴛ', 'ⴛ');
    UPPERCASE_CONVERSION.put('ⴜ', 'ⴜ');
    UPPERCASE_CONVERSION.put('ⴝ', 'ⴝ');
    UPPERCASE_CONVERSION.put('ⴞ', 'ⴞ');
    UPPERCASE_CONVERSION.put('ⴟ', 'ⴟ');
    UPPERCASE_CONVERSION.put('ⴠ', 'ⴠ');
    UPPERCASE_CONVERSION.put('ⴡ', 'ⴡ');
    UPPERCASE_CONVERSION.put('ⴢ', 'ⴢ');
    UPPERCASE_CONVERSION.put('ⴣ', 'ⴣ');
    UPPERCASE_CONVERSION.put('ⴤ', 'ⴤ');
    UPPERCASE_CONVERSION.put('ⴥ', 'ⴥ');
    UPPERCASE_CONVERSION.put('ⴧ', 'ⴧ');
    UPPERCASE_CONVERSION.put('ⴭ', 'ⴭ');
    UPPERCASE_CONVERSION.put('ꙁ', 'ꙁ');
    UPPERCASE_CONVERSION.put('ꙃ', 'ꙃ');
    UPPERCASE_CONVERSION.put('ꙅ', 'ꙅ');
    UPPERCASE_CONVERSION.put('ꙇ', 'ꙇ');
    UPPERCASE_CONVERSION.put('ꙉ', 'ꙉ');
    UPPERCASE_CONVERSION.put('ꙋ', 'ꙋ');
    UPPERCASE_CONVERSION.put('ꙍ', 'ꙍ');
    UPPERCASE_CONVERSION.put('ꙏ', 'ꙏ');
    UPPERCASE_CONVERSION.put('ꙑ', 'ꙑ');
    UPPERCASE_CONVERSION.put('ꙓ', 'ꙓ');
    UPPERCASE_CONVERSION.put('ꙕ', 'ꙕ');
    UPPERCASE_CONVERSION.put('ꙗ', 'ꙗ');
    UPPERCASE_CONVERSION.put('ꙙ', 'ꙙ');
    UPPERCASE_CONVERSION.put('ꙛ', 'ꙛ');
    UPPERCASE_CONVERSION.put('ꙝ', 'ꙝ');
    UPPERCASE_CONVERSION.put('ꙟ', 'ꙟ');
    UPPERCASE_CONVERSION.put('ꙡ', 'ꙡ');
    UPPERCASE_CONVERSION.put('ꙣ', 'ꙣ');
    UPPERCASE_CONVERSION.put('ꙥ', 'ꙥ');
    UPPERCASE_CONVERSION.put('ꙧ', 'ꙧ');
    UPPERCASE_CONVERSION.put('ꙩ', 'ꙩ');
    UPPERCASE_CONVERSION.put('ꙫ', 'ꙫ');
    UPPERCASE_CONVERSION.put('ꙭ', 'ꙭ');
    UPPERCASE_CONVERSION.put('ꚁ', 'ꚁ');
    UPPERCASE_CONVERSION.put('ꚃ', 'ꚃ');
    UPPERCASE_CONVERSION.put('ꚅ', 'ꚅ');
    UPPERCASE_CONVERSION.put('ꚇ', 'ꚇ');
    UPPERCASE_CONVERSION.put('ꚉ', 'ꚉ');
    UPPERCASE_CONVERSION.put('ꚋ', 'ꚋ');
    UPPERCASE_CONVERSION.put('ꚍ', 'ꚍ');
    UPPERCASE_CONVERSION.put('ꚏ', 'ꚏ');
    UPPERCASE_CONVERSION.put('ꚑ', 'ꚑ');
    UPPERCASE_CONVERSION.put('ꚓ', 'ꚓ');
    UPPERCASE_CONVERSION.put('ꚕ', 'ꚕ');
    UPPERCASE_CONVERSION.put('ꚗ', 'ꚗ');
    UPPERCASE_CONVERSION.put('ꚙ', 'ꚙ');
    UPPERCASE_CONVERSION.put('ꚛ', 'ꚛ');
    UPPERCASE_CONVERSION.put('ꜣ', 'ꜣ');
    UPPERCASE_CONVERSION.put('ꜥ', 'ꜥ');
    UPPERCASE_CONVERSION.put('ꜧ', 'ꜧ');
    UPPERCASE_CONVERSION.put('ꜩ', 'ꜩ');
    UPPERCASE_CONVERSION.put('ꜫ', 'ꜫ');
    UPPERCASE_CONVERSION.put('ꜭ', 'ꜭ');
    UPPERCASE_CONVERSION.put('ꜯ', 'ꜯ');
    UPPERCASE_CONVERSION.put('ꜳ', 'ꜳ');
    UPPERCASE_CONVERSION.put('ꜵ', 'ꜵ');
    UPPERCASE_CONVERSION.put('ꜷ', 'ꜷ');
    UPPERCASE_CONVERSION.put('ꜹ', 'ꜹ');
    UPPERCASE_CONVERSION.put('ꜻ', 'ꜻ');
    UPPERCASE_CONVERSION.put('ꜽ', 'ꜽ');
    UPPERCASE_CONVERSION.put('ꜿ', 'ꜿ');
    UPPERCASE_CONVERSION.put('ꝁ', 'ꝁ');
    UPPERCASE_CONVERSION.put('ꝃ', 'ꝃ');
    UPPERCASE_CONVERSION.put('ꝅ', 'ꝅ');
    UPPERCASE_CONVERSION.put('ꝇ', 'ꝇ');
    UPPERCASE_CONVERSION.put('ꝉ', 'ꝉ');
    UPPERCASE_CONVERSION.put('ꝋ', 'ꝋ');
    UPPERCASE_CONVERSION.put('ꝍ', 'ꝍ');
    UPPERCASE_CONVERSION.put('ꝏ', 'ꝏ');
    UPPERCASE_CONVERSION.put('ꝑ', 'ꝑ');
    UPPERCASE_CONVERSION.put('ꝓ', 'ꝓ');
    UPPERCASE_CONVERSION.put('ꝕ', 'ꝕ');
    UPPERCASE_CONVERSION.put('ꝗ', 'ꝗ');
    UPPERCASE_CONVERSION.put('ꝙ', 'ꝙ');
    UPPERCASE_CONVERSION.put('ꝛ', 'ꝛ');
    UPPERCASE_CONVERSION.put('ꝝ', 'ꝝ');
    UPPERCASE_CONVERSION.put('ꝟ', 'ꝟ');
    UPPERCASE_CONVERSION.put('ꝡ', 'ꝡ');
    UPPERCASE_CONVERSION.put('ꝣ', 'ꝣ');
    UPPERCASE_CONVERSION.put('ꝥ', 'ꝥ');
    UPPERCASE_CONVERSION.put('ꝧ', 'ꝧ');
    UPPERCASE_CONVERSION.put('ꝩ', 'ꝩ');
    UPPERCASE_CONVERSION.put('ꝫ', 'ꝫ');
    UPPERCASE_CONVERSION.put('ꝭ', 'ꝭ');
    UPPERCASE_CONVERSION.put('ꝯ', 'ꝯ');
    UPPERCASE_CONVERSION.put('ꝺ', 'ꝺ');
    UPPERCASE_CONVERSION.put('ꝼ', 'ꝼ');
    UPPERCASE_CONVERSION.put('ꝿ', 'ꝿ');
    UPPERCASE_CONVERSION.put('ꞁ', 'ꞁ');
    UPPERCASE_CONVERSION.put('ꞃ', 'ꞃ');
    UPPERCASE_CONVERSION.put('ꞅ', 'ꞅ');
    UPPERCASE_CONVERSION.put('ꞇ', 'ꞇ');
    UPPERCASE_CONVERSION.put('ꞌ', 'ꞌ');
    UPPERCASE_CONVERSION.put('ꞑ', 'ꞑ');
    UPPERCASE_CONVERSION.put('ꞓ', 'ꞓ');
    UPPERCASE_CONVERSION.put('ꞗ', 'ꞗ');
    UPPERCASE_CONVERSION.put('ꞙ', 'ꞙ');
    UPPERCASE_CONVERSION.put('ꞛ', 'ꞛ');
    UPPERCASE_CONVERSION.put('ꞝ', 'ꞝ');
    UPPERCASE_CONVERSION.put('ꞟ', 'ꞟ');
    UPPERCASE_CONVERSION.put('ꞡ', 'ꞡ');
    UPPERCASE_CONVERSION.put('ꞣ', 'ꞣ');
    UPPERCASE_CONVERSION.put('ꞥ', 'ꞥ');
    UPPERCASE_CONVERSION.put('ꞧ', 'ꞧ');
    UPPERCASE_CONVERSION.put('ꞩ', 'ꞩ');
    UPPERCASE_CONVERSION.put('ꞵ', 'ꞵ');
    UPPERCASE_CONVERSION.put('ꞷ', 'ꞷ');
    UPPERCASE_CONVERSION.put('ꭓ', 'ꭓ');
    UPPERCASE_CONVERSION.put('ꭰ', 'ꭰ');
    UPPERCASE_CONVERSION.put('ꭱ', 'ꭱ');
    UPPERCASE_CONVERSION.put('ꭲ', 'ꭲ');
    UPPERCASE_CONVERSION.put('ꭳ', 'ꭳ');
    UPPERCASE_CONVERSION.put('ꭴ', 'ꭴ');
    UPPERCASE_CONVERSION.put('ꭵ', 'ꭵ');
    UPPERCASE_CONVERSION.put('ꭶ', 'ꭶ');
    UPPERCASE_CONVERSION.put('ꭷ', 'ꭷ');
    UPPERCASE_CONVERSION.put('ꭸ', 'ꭸ');
    UPPERCASE_CONVERSION.put('ꭹ', 'ꭹ');
    UPPERCASE_CONVERSION.put('ꭺ', 'ꭺ');
    UPPERCASE_CONVERSION.put('ꭻ', 'ꭻ');
    UPPERCASE_CONVERSION.put('ꭼ', 'ꭼ');
    UPPERCASE_CONVERSION.put('ꭽ', 'ꭽ');
    UPPERCASE_CONVERSION.put('ꭾ', 'ꭾ');
    UPPERCASE_CONVERSION.put('ꭿ', 'ꭿ');
    UPPERCASE_CONVERSION.put('ꮀ', 'ꮀ');
    UPPERCASE_CONVERSION.put('ꮁ', 'ꮁ');
    UPPERCASE_CONVERSION.put('ꮂ', 'ꮂ');
    UPPERCASE_CONVERSION.put('ꮃ', 'ꮃ');
    UPPERCASE_CONVERSION.put('ꮄ', 'ꮄ');
    UPPERCASE_CONVERSION.put('ꮅ', 'ꮅ');
    UPPERCASE_CONVERSION.put('ꮆ', 'ꮆ');
    UPPERCASE_CONVERSION.put('ꮇ', 'ꮇ');
    UPPERCASE_CONVERSION.put('ꮈ', 'ꮈ');
    UPPERCASE_CONVERSION.put('ꮉ', 'ꮉ');
    UPPERCASE_CONVERSION.put('ꮊ', 'ꮊ');
    UPPERCASE_CONVERSION.put('ꮋ', 'ꮋ');
    UPPERCASE_CONVERSION.put('ꮌ', 'ꮌ');
    UPPERCASE_CONVERSION.put('ꮍ', 'ꮍ');
    UPPERCASE_CONVERSION.put('ꮎ', 'ꮎ');
    UPPERCASE_CONVERSION.put('ꮏ', 'ꮏ');
    UPPERCASE_CONVERSION.put('ꮐ', 'ꮐ');
    UPPERCASE_CONVERSION.put('ꮑ', 'ꮑ');
    UPPERCASE_CONVERSION.put('ꮒ', 'ꮒ');
    UPPERCASE_CONVERSION.put('ꮓ', 'ꮓ');
    UPPERCASE_CONVERSION.put('ꮔ', 'ꮔ');
    UPPERCASE_CONVERSION.put('ꮕ', 'ꮕ');
    UPPERCASE_CONVERSION.put('ꮖ', 'ꮖ');
    UPPERCASE_CONVERSION.put('ꮗ', 'ꮗ');
    UPPERCASE_CONVERSION.put('ꮘ', 'ꮘ');
    UPPERCASE_CONVERSION.put('ꮙ', 'ꮙ');
    UPPERCASE_CONVERSION.put('ꮚ', 'ꮚ');
    UPPERCASE_CONVERSION.put('ꮛ', 'ꮛ');
    UPPERCASE_CONVERSION.put('ꮜ', 'ꮜ');
    UPPERCASE_CONVERSION.put('ꮝ', 'ꮝ');
    UPPERCASE_CONVERSION.put('ꮞ', 'ꮞ');
    UPPERCASE_CONVERSION.put('ꮟ', 'ꮟ');
    UPPERCASE_CONVERSION.put('ꮠ', 'ꮠ');
    UPPERCASE_CONVERSION.put('ꮡ', 'ꮡ');
    UPPERCASE_CONVERSION.put('ꮢ', 'ꮢ');
    UPPERCASE_CONVERSION.put('ꮣ', 'ꮣ');
    UPPERCASE_CONVERSION.put('ꮤ', 'ꮤ');
    UPPERCASE_CONVERSION.put('ꮥ', 'ꮥ');
    UPPERCASE_CONVERSION.put('ꮦ', 'ꮦ');
    UPPERCASE_CONVERSION.put('ꮧ', 'ꮧ');
    UPPERCASE_CONVERSION.put('ꮨ', 'ꮨ');
    UPPERCASE_CONVERSION.put('ꮩ', 'ꮩ');
    UPPERCASE_CONVERSION.put('ꮪ', 'ꮪ');
    UPPERCASE_CONVERSION.put('ꮫ', 'ꮫ');
    UPPERCASE_CONVERSION.put('ꮬ', 'ꮬ');
    UPPERCASE_CONVERSION.put('ꮭ', 'ꮭ');
    UPPERCASE_CONVERSION.put('ꮮ', 'ꮮ');
    UPPERCASE_CONVERSION.put('ꮯ', 'ꮯ');
    UPPERCASE_CONVERSION.put('ꮰ', 'ꮰ');
    UPPERCASE_CONVERSION.put('ꮱ', 'ꮱ');
    UPPERCASE_CONVERSION.put('ꮲ', 'ꮲ');
    UPPERCASE_CONVERSION.put('ꮳ', 'ꮳ');
    UPPERCASE_CONVERSION.put('ꮴ', 'ꮴ');
    UPPERCASE_CONVERSION.put('ꮵ', 'ꮵ');
    UPPERCASE_CONVERSION.put('ꮶ', 'ꮶ');
    UPPERCASE_CONVERSION.put('ꮷ', 'ꮷ');
    UPPERCASE_CONVERSION.put('ꮸ', 'ꮸ');
    UPPERCASE_CONVERSION.put('ꮹ', 'ꮹ');
    UPPERCASE_CONVERSION.put('ꮺ', 'ꮺ');
    UPPERCASE_CONVERSION.put('ꮻ', 'ꮻ');
    UPPERCASE_CONVERSION.put('ꮼ', 'ꮼ');
    UPPERCASE_CONVERSION.put('ꮽ', 'ꮽ');
    UPPERCASE_CONVERSION.put('ꮾ', 'ꮾ');
    UPPERCASE_CONVERSION.put('ꮿ', 'ꮿ');
    // UPPERCASE_CONVERSION.put('𐑎', '𐑎');
    // UPPERCASE_CONVERSION.put('𐑏', '𐑏');
    // UPPERCASE_CONVERSION.put('𐓘', '𐓘');
    // UPPERCASE_CONVERSION.put('𐓙', '𐓙');
    // UPPERCASE_CONVERSION.put('𐓚', '𐓚');
    // UPPERCASE_CONVERSION.put('𐓛', '𐓛');
    // UPPERCASE_CONVERSION.put('𐓜', '𐓜');
    // UPPERCASE_CONVERSION.put('𐓝', '𐓝');
    // UPPERCASE_CONVERSION.put('𐓞', '𐓞');
    // UPPERCASE_CONVERSION.put('𐓟', '𐓟');
    // UPPERCASE_CONVERSION.put('𐓠', '𐓠');
    // UPPERCASE_CONVERSION.put('𐓡', '𐓡');
    // UPPERCASE_CONVERSION.put('𐓢', '𐓢');
    // UPPERCASE_CONVERSION.put('𐓣', '𐓣');
    // UPPERCASE_CONVERSION.put('𐓤', '𐓤');
    // UPPERCASE_CONVERSION.put('𐓥', '𐓥');
    // UPPERCASE_CONVERSION.put('𐓦', '𐓦');
    // UPPERCASE_CONVERSION.put('𐓧', '𐓧');
    // UPPERCASE_CONVERSION.put('𐓨', '𐓨');
    // UPPERCASE_CONVERSION.put('𐓩', '𐓩');
    // UPPERCASE_CONVERSION.put('𐓪', '𐓪');
    // UPPERCASE_CONVERSION.put('𐓫', '𐓫');
    // UPPERCASE_CONVERSION.put('𐓬', '𐓬');
    // UPPERCASE_CONVERSION.put('𐓭', '𐓭');
    // UPPERCASE_CONVERSION.put('𐓮', '𐓮');
    // UPPERCASE_CONVERSION.put('𐓯', '𐓯');
    // UPPERCASE_CONVERSION.put('𐓰', '𐓰');
    // UPPERCASE_CONVERSION.put('𐓱', '𐓱');
    // UPPERCASE_CONVERSION.put('𐓲', '𐓲');
    // UPPERCASE_CONVERSION.put('𐓳', '𐓳');
    // UPPERCASE_CONVERSION.put('𐓴', '𐓴');
    // UPPERCASE_CONVERSION.put('𐓵', '𐓵');
    // UPPERCASE_CONVERSION.put('𐓶', '𐓶');
    // UPPERCASE_CONVERSION.put('𐓷', '𐓷');
    // UPPERCASE_CONVERSION.put('𐓸', '𐓸');
    // UPPERCASE_CONVERSION.put('𐓹', '𐓹');
    // UPPERCASE_CONVERSION.put('𐓺', '𐓺');
    // UPPERCASE_CONVERSION.put('𐓻', '𐓻');
    // UPPERCASE_CONVERSION.put('𐳀', '𐳀');
    // UPPERCASE_CONVERSION.put('𐳁', '𐳁');
    // UPPERCASE_CONVERSION.put('𐳂', '𐳂');
    // UPPERCASE_CONVERSION.put('𐳃', '𐳃');
    // UPPERCASE_CONVERSION.put('𐳄', '𐳄');
    // UPPERCASE_CONVERSION.put('𐳅', '𐳅');
    // UPPERCASE_CONVERSION.put('𐳆', '𐳆');
    // UPPERCASE_CONVERSION.put('𐳇', '𐳇');
    // UPPERCASE_CONVERSION.put('𐳈', '𐳈');
    // UPPERCASE_CONVERSION.put('𐳉', '𐳉');
    // UPPERCASE_CONVERSION.put('𐳊', '𐳊');
    // UPPERCASE_CONVERSION.put('𐳋', '𐳋');
    // UPPERCASE_CONVERSION.put('𐳌', '𐳌');
    // UPPERCASE_CONVERSION.put('𐳍', '𐳍');
    // UPPERCASE_CONVERSION.put('𐳎', '𐳎');
    // UPPERCASE_CONVERSION.put('𐳏', '𐳏');
    // UPPERCASE_CONVERSION.put('𐳐', '𐳐');
    // UPPERCASE_CONVERSION.put('𐳑', '𐳑');
    // UPPERCASE_CONVERSION.put('𐳒', '𐳒');
    // UPPERCASE_CONVERSION.put('𐳓', '𐳓');
    // UPPERCASE_CONVERSION.put('𐳔', '𐳔');
    // UPPERCASE_CONVERSION.put('𐳕', '𐳕');
    // UPPERCASE_CONVERSION.put('𐳖', '𐳖');
    // UPPERCASE_CONVERSION.put('𐳗', '𐳗');
    // UPPERCASE_CONVERSION.put('𐳘', '𐳘');
    // UPPERCASE_CONVERSION.put('𐳙', '𐳙');
    // UPPERCASE_CONVERSION.put('𐳚', '𐳚');
    // UPPERCASE_CONVERSION.put('𐳛', '𐳛');
    // UPPERCASE_CONVERSION.put('𐳜', '𐳜');
    // UPPERCASE_CONVERSION.put('𐳝', '𐳝');
    // UPPERCASE_CONVERSION.put('𐳞', '𐳞');
    // UPPERCASE_CONVERSION.put('𐳟', '𐳟');
    // UPPERCASE_CONVERSION.put('𐳠', '𐳠');
    // UPPERCASE_CONVERSION.put('𐳡', '𐳡');
    // UPPERCASE_CONVERSION.put('𐳢', '𐳢');
    // UPPERCASE_CONVERSION.put('𐳣', '𐳣');
    // UPPERCASE_CONVERSION.put('𐳤', '𐳤');
    // UPPERCASE_CONVERSION.put('𐳥', '𐳥');
    // UPPERCASE_CONVERSION.put('𐳦', '𐳦');
    // UPPERCASE_CONVERSION.put('𐳧', '𐳧');
    // UPPERCASE_CONVERSION.put('𐳨', '𐳨');
    // UPPERCASE_CONVERSION.put('𐳩', '𐳩');
    // UPPERCASE_CONVERSION.put('𐳪', '𐳪');
    // UPPERCASE_CONVERSION.put('𐳫', '𐳫');
    // UPPERCASE_CONVERSION.put('𐳬', '𐳬');
    // UPPERCASE_CONVERSION.put('𐳭', '𐳭');
    // UPPERCASE_CONVERSION.put('𐳮', '𐳮');
    // UPPERCASE_CONVERSION.put('𐳯', '𐳯');
    // UPPERCASE_CONVERSION.put('𐳰', '𐳰');
    // UPPERCASE_CONVERSION.put('𐳱', '𐳱');
    // UPPERCASE_CONVERSION.put('𐳲', '𐳲');
    // UPPERCASE_CONVERSION.put('𑣀', '𑣀');
    // UPPERCASE_CONVERSION.put('𑣁', '𑣁');
    // UPPERCASE_CONVERSION.put('𑣂', '𑣂');
    // UPPERCASE_CONVERSION.put('𑣃', '𑣃');
    // UPPERCASE_CONVERSION.put('𑣄', '𑣄');
    // UPPERCASE_CONVERSION.put('𑣅', '𑣅');
    // UPPERCASE_CONVERSION.put('𑣆', '𑣆');
    // UPPERCASE_CONVERSION.put('𑣇', '𑣇');
    // UPPERCASE_CONVERSION.put('𑣈', '𑣈');
    // UPPERCASE_CONVERSION.put('𑣉', '𑣉');
    // UPPERCASE_CONVERSION.put('𑣊', '𑣊');
    // UPPERCASE_CONVERSION.put('𑣋', '𑣋');
    // UPPERCASE_CONVERSION.put('𑣌', '𑣌');
    // UPPERCASE_CONVERSION.put('𑣍', '𑣍');
    // UPPERCASE_CONVERSION.put('𑣎', '𑣎');
    // UPPERCASE_CONVERSION.put('𑣏', '𑣏');
    // UPPERCASE_CONVERSION.put('𑣐', '𑣐');
    // UPPERCASE_CONVERSION.put('𑣑', '𑣑');
    // UPPERCASE_CONVERSION.put('𑣒', '𑣒');
    // UPPERCASE_CONVERSION.put('𑣓', '𑣓');
    // UPPERCASE_CONVERSION.put('𑣔', '𑣔');
    // UPPERCASE_CONVERSION.put('𑣕', '𑣕');
    // UPPERCASE_CONVERSION.put('𑣖', '𑣖');
    // UPPERCASE_CONVERSION.put('𑣗', '𑣗');
    // UPPERCASE_CONVERSION.put('𑣘', '𑣘');
    // UPPERCASE_CONVERSION.put('𑣙', '𑣙');
    // UPPERCASE_CONVERSION.put('𑣚', '𑣚');
    // UPPERCASE_CONVERSION.put('𑣛', '𑣛');
    // UPPERCASE_CONVERSION.put('𑣜', '𑣜');
    // UPPERCASE_CONVERSION.put('𑣝', '𑣝');
    // UPPERCASE_CONVERSION.put('𑣞', '𑣞');
    // UPPERCASE_CONVERSION.put('𑣟', '𑣟');
    // UPPERCASE_CONVERSION.put('𞤢', '𞤢');
    // UPPERCASE_CONVERSION.put('𞤣', '𞤣');
    // UPPERCASE_CONVERSION.put('𞤤', '𞤤');
    // UPPERCASE_CONVERSION.put('𞤥', '𞤥');
    // UPPERCASE_CONVERSION.put('𞤦', '𞤦');
    // UPPERCASE_CONVERSION.put('𞤧', '𞤧');
    // UPPERCASE_CONVERSION.put('𞤨', '𞤨');
    // UPPERCASE_CONVERSION.put('𞤩', '𞤩');
    // UPPERCASE_CONVERSION.put('𞤪', '𞤪');
    // UPPERCASE_CONVERSION.put('𞤫', '𞤫');
    // UPPERCASE_CONVERSION.put('𞤬', '𞤬');
    // UPPERCASE_CONVERSION.put('𞤭', '𞤭');
    // UPPERCASE_CONVERSION.put('𞤮', '𞤮');
    // UPPERCASE_CONVERSION.put('𞤯', '𞤯');
    // UPPERCASE_CONVERSION.put('𞤰', '𞤰');
    // UPPERCASE_CONVERSION.put('𞤱', '𞤱');
    // UPPERCASE_CONVERSION.put('𞤲', '𞤲');
    // UPPERCASE_CONVERSION.put('𞤳', '𞤳');
    // UPPERCASE_CONVERSION.put('𞤴', '𞤴');
    // UPPERCASE_CONVERSION.put('𞤵', '𞤵');
    // UPPERCASE_CONVERSION.put('𞤶', '𞤶');
    // UPPERCASE_CONVERSION.put('𞤷', '𞤷');
    // UPPERCASE_CONVERSION.put('𞤸', '𞤸');
    // UPPERCASE_CONVERSION.put('𞤹', '𞤹');
    // UPPERCASE_CONVERSION.put('𞤺', '𞤺');
    // UPPERCASE_CONVERSION.put('𞤻', '𞤻');
    // UPPERCASE_CONVERSION.put('𞤼', '𞤼');
    // UPPERCASE_CONVERSION.put('𞤽', '𞤽');
    // UPPERCASE_CONVERSION.put('𞤾', '𞤾');
    // UPPERCASE_CONVERSION.put('𞤿', '𞤿');
    // UPPERCASE_CONVERSION.put('𞥀', '𞥀');
    // UPPERCASE_CONVERSION.put('𞥁', '𞥁');
    // UPPERCASE_CONVERSION.put('𞥂', '𞥂');
    // UPPERCASE_CONVERSION.put('𞥃', '𞥃');

    // Extra conversion
    UPPERCASE_CONVERSION.put('ⅷ', 'ⅷ');

    // Compute boundaries
    int lower = Integer.MAX_VALUE;
    int upper = Integer.MIN_VALUE;
    for (Character character : UPPERCASE_CONVERSION.keySet()) {
      if (character < lower) {
        lower = character;
      }
      if (character > upper) {
        upper = character;
      }
    }
    LOWER_BOUNDARY = lower;
    UPPER_BOUNDARY = upper;
  }
}
