/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2021  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm.a5xx.a56x.a567;

import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.HtmlCharacters;
import org.wikipediacleaner.api.data.PageElementFunction;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.ContentsUtil;
import org.wikipediacleaner.api.data.contents.comment.ContentsComment;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;
import org.wikipediacleaner.utils.string.CharacterUtils;

/**
 * Manage formatnum error detection and fixing
 */
class Formatnum {

  private final PageAnalysis analysis;
  private final PageElementFunction function;
  private final int initialBeginValue;
  private final int initialEndValue;
  private final List<String> prefixes;
  private final List<String> suffixes;

  private int beginValue;
  private int endValue;
  private String prefix;
  private String value;
  private String suffix;
  private boolean automatic;

  public Formatnum(
      PageAnalysis analysis,
      PageElementFunction function,
      List<String> prefixes,
      List<String> suffixes) {
    this.analysis = analysis;
    this.function = function;
    this.initialBeginValue = function.getParameterValueNotTrimmedOffset(0);
    this.initialEndValue = (function.getParameterCount() > 1) ?
        function.getParameterSeparatorOffset(1) : function.getEndIndex() - 2;
    this.prefixes = prefixes;
    this.suffixes = suffixes;

    this.beginValue = initialBeginValue;
    this.endValue = initialEndValue;
    this.prefix = StringUtils.EMPTY;
    this.value = function.getParameterValueNotTrimmed(0);
    this.suffix = StringUtils.EMPTY;
    this.automatic = true;
  }

  /**
   * Add suggestions for an invalid formatnum:
   * 
   * @param errorResult
   */
  public void addSuggestions(CheckErrorResult errorResult) {
    boolean tryAgain = true;
    while (tryAgain) {
      tryAgain = false;
      tryAgain |= extractBoldItalic();
      tryAgain |= extractLeadingText();
      tryAgain |= extractTrailingText();
      tryAgain |= removeSurroundingWhitespace();
      tryAgain |= extractReference();
      tryAgain |= replaceIncorrectMinus();
      tryAgain |= removeWhitespaceBetweenDigits();
      tryAgain |= removeThousandSeparators();
      tryAgain |= replaceCommaByDot();
      tryAgain |= extractNoDigitsText();
    }

    // Do not manage empty formatnum:
    if (value.isEmpty() && prefix.isEmpty() && suffix.isEmpty()) {
      return;
    }

    // Remove formatnum: if the result is empty
    if (value.isEmpty()) {
      // Compute some intermediary values
      String fullText = prefix + suffix;
      char firstChar = fullText.charAt(0);
      char lastChar = fullText.charAt(fullText.length() - 1);
      String contents = analysis.getContents();
      int functionBegin = function.getBeginIndex();
      char previousChar = (functionBegin > 0) ? contents.charAt(functionBegin - 1) : '\n';
      int functionEnd = function.getEndIndex();
      char nextChar = (functionEnd < contents.length()) ? contents.charAt(functionEnd) : '\n';

      // Check if replacement can be automatic
      automatic &= (previousChar != '\'') || (firstChar != '\'');
      automatic &= (nextChar != '\'') || (lastChar != '\'');
      automatic &= (previousChar != '|') || (firstChar != '|');
      automatic &= (nextChar != '|') || (lastChar != '|');
      if ((previousChar == '|') && (firstChar == '-')) {
        fullText = ' ' + fullText; // Prevent |-
      }
      errorResult.addReplacement(fullText, automatic);
      return;
    }

    // Try to remove all trailing extra text
    if (!isValidFormatnum(analysis, value, beginValue)) {
      int tmpIndex = value.length();
      while ((tmpIndex > 0) &&
             ("0123456789+-".indexOf(value.charAt(tmpIndex - 1)) < 0)) {
        if ((value.charAt(tmpIndex - 1) == '\'') &&
            (tmpIndex > 1) &&
            (value.charAt(tmpIndex - 2) == '\'')) {
          break;
        }
        tmpIndex--;
      }
      if (tmpIndex < value.length()) {
        suffix = value.substring(tmpIndex) + suffix;
        endValue += (value.length() - tmpIndex);
        value = value.substring(0, tmpIndex);
        automatic = false;
      }
    }

    // Replace if the new value is valid
    if (isValidFormatnum(analysis, value, beginValue)) {
      String contents = analysis.getContents();
      String functionPrefix = contents.substring(function.getBeginIndex(), initialBeginValue);
      String functionSuffix = contents.substring(initialEndValue, function.getEndIndex());
      errorResult.addReplacement(
          prefix + functionPrefix + value + functionSuffix + suffix,
          automatic);
      return;
    }
  }

  private static final String WHITESPACE =
      " \t" +
      HtmlCharacters.NARROW_NO_BREAK_SPACE.getValue() +
      HtmlCharacters.SYMBOL_NON_BREAKING_SPACE.getValue() +
      HtmlCharacters.THIN_SPACE.getValue() +
      HtmlCharacters.HAIR_SPACE.getValue();
  private static final String BEFORE_WHITESPACE =
      "0123456789+-";
  private static final String AFTER_WHITESPACE =
      "0123456789 " +
      HtmlCharacters.NARROW_NO_BREAK_SPACE.getValue() +
      HtmlCharacters.SYMBOL_NON_BREAKING_SPACE.getValue() +
      HtmlCharacters.THIN_SPACE.getValue() +
      HtmlCharacters.HAIR_SPACE.getValue();

  /**
   * @return True if whitespace was removed between digits.
   */
  private boolean removeWhitespaceBetweenDigits() {
    if (value.isEmpty()) {
      return false;
    }
    char previousChar = value.charAt(0);
    int tmpIndex = 1;
    while (tmpIndex < value.length() - 1) {
      char currentChar = value.charAt(tmpIndex);
      if (WHITESPACE.indexOf(currentChar) >= 0) {
        if ((BEFORE_WHITESPACE.indexOf(previousChar) >= 0) &&
            (AFTER_WHITESPACE.indexOf(value.charAt(tmpIndex + 1)) >= 0)) {
          value = value.substring(0, tmpIndex) + value.substring(tmpIndex + 1);
          return true;
        }
      }
      previousChar = currentChar;
      tmpIndex++;
    }
    return false;
  }

  /**
   * @return True if thousand separators were removed.
   */
  private boolean removeThousandSeparators() {
    if (value.isEmpty()) {
      return false;
    }
    int beginDigit = ContentsUtil.moveIndexForwardWhileFound(value, 0, "+-");
    int endDigit = ContentsUtil.moveIndexForwardWhileFound(value, beginDigit, "0123456789");
    if ((endDigit == beginDigit) || (endDigit >= value.length())) {
      return false;
    }
    char separator = value.charAt(endDigit);
    if (",.".indexOf(separator) < 0) {
      return false;
    }
    StringBuilder tmp = new StringBuilder(value.substring(0, endDigit));
    int commaCount = 0;
    boolean endWith0 = false;
    while ((endDigit < value.length()) && (value.charAt(endDigit) == separator)) {
      beginDigit = endDigit + 1;
      endDigit = ContentsUtil.moveIndexForwardWhileFound(value, beginDigit, "0123456789");
      if (endDigit != beginDigit + 3) {
        return false;
      }
      String digits = value.substring(beginDigit, endDigit);
      endWith0 = digits.endsWith("00");
      tmp.append(digits);
      commaCount++;
    }
    if (endDigit < value.length()) {
      return false;
    }
    if (commaCount < 2) {
      if (separator == ',') {
        if (!endWith0) {
          return false;
        }
      } else {
        return false;
      }
    }
    value = tmp.toString();
    return true;
  }

  /**
   * @return True if the comma has been replaced by a dot.
   */
  private boolean replaceCommaByDot() {
    // Check if we have digits + comma + no more than 2 digits
    if (value.isEmpty()) {
      return false;
    }
    int beginDigit = ContentsUtil.moveIndexForwardWhileFound(value, 0, "+-");
    int endDigit = ContentsUtil.moveIndexForwardWhileFound(value, beginDigit, "0123456789");
    if ((endDigit == beginDigit) ||
        (endDigit >= value.length()) ||
        (value.charAt(endDigit) != ',')) {
      return false;
    }
    int endDigit2 = ContentsUtil.moveIndexForwardWhileFound(value, endDigit + 1, "0123456789");
    if ((endDigit2 == endDigit + 1) ||
        (endDigit2 < value.length())) {
      return false;
    }
    if ((endDigit2 > endDigit + 3) && (endDigit - beginDigit <= 3)) {
      return false;
    }

    // Replace comma by dot
    value = value.substring(0, endDigit) + "." + value.substring(endDigit + 1);
    return true;
  }

  /**
   * @return True if surrounding whitespace was removed between digits.
   */
  private boolean removeSurroundingWhitespace() {
    if (value.isEmpty()) {
      return false;
    }
    if ((WHITESPACE.indexOf(value.charAt(0)) < 0) &&
        (WHITESPACE.indexOf(value.charAt(value.length() - 1)) < 0)) {
      return false;
    }

    // Remove whitespace at the beginning
    String contents = analysis.getContents();
    int beginIndex = function.getBeginIndex();
    boolean emptyPrefix = prefix.isEmpty();
    char previousChar = beginIndex > 0 ? contents.charAt(beginIndex - 1) : '\n';
    while (!value.isEmpty() && (WHITESPACE.indexOf(value.charAt(0)) >= 0)) {
      if (!emptyPrefix || (" \n".indexOf(previousChar) < 0)) {
        prefix += value.charAt(0);
      }
      value = value.substring(1);
      beginValue++;
    }

    // Remove whitespace at the end
    int endIndex = function.getEndIndex();
    boolean emptySuffix = suffix.isEmpty();
    char nextChar = endIndex < contents.length() ? contents.charAt(endIndex) : '\n';
    while (!value.isEmpty() && (WHITESPACE.indexOf(value.charAt(value.length() - 1)) >= 0)) {
      if (!emptySuffix || (" \n".indexOf(nextChar) < 0)) {
        suffix = " " + suffix;
      }
      value = value.substring(0, value.length() - 1);
      endValue--;
    }
    return true;
  }

  /**
   * @return True if incorrect minus sign was replaced.
   */
  private boolean replaceIncorrectMinus() {
    if (value.isEmpty()) {
      return false;
    }
    int firstChar = value.charAt(0);
    if ("–\u2212".indexOf(firstChar) < 0) {
      return false;
    }

    // Replace with minus sign
    value = "-" + value.substring(1);
    return true;
  }

  /**
   * @return True if leading text was extracted.
   */
  private boolean extractLeadingText() {
    if (value.isEmpty()) {
      return false;
    }

    // Remove leading text
    for (String testPrefix : prefixes) {
      if (value.startsWith(testPrefix) &&
          (value.length() > testPrefix.length()) &&
          (!CharacterUtils.isAsciiLetter(value.charAt(testPrefix.length())))) {
        prefix += testPrefix;
        value = value.substring(testPrefix.length());
        beginValue += testPrefix.length();
        return true;
      }
    }

    // Remove leading character
    char firstChar = value.charAt(0);
    if ("($€£₤~〜>≈±\"".indexOf(firstChar) >= 0) {
      prefix += firstChar;
      value = value.substring(1);
      beginValue++;
      return true;
    }

    return false;
  }

  /**
   * @return True if trailing text was extracted.
   */
  private boolean extractTrailingText() {
    if (value.isEmpty()) {
      return false;
    }

    // Remove known trailing words
    for (String testSuffix : suffixes) {
      if (value.endsWith(testSuffix) &&
          (value.length() > testSuffix.length()) &&
          (!CharacterUtils.isAsciiLetter(value.charAt(value.length() - testSuffix.length() - 1)))) {
        suffix = testSuffix + suffix;
        value = value.substring(0, value.length() - testSuffix.length());
        endValue -= testSuffix.length();
        return true;
      }
    }

    // Remove trailing character
    char lastChar = value.charAt(value.length() - 1);
    if (")%$€£₤?+*:\"".indexOf(lastChar) >= 0) {
      suffix = lastChar + suffix;
      value = value.substring(0, value.length() - 1);
      endValue--;
      return true;
    }

    return false;
  }

  /**
   * @return True if a reference was extracted from the text.
   */
  private boolean extractReference() {
    if (value.isEmpty()) {
      return false;
    }

    // Check if a reference is at the end
    char lastChar = value.charAt(value.length() - 1);
    if (lastChar != '>') {
      return false;
    }
    PageElementTag tag = analysis.isInTag(endValue - 1, WikiTagType.REF);
    if ((tag == null) ||
        (tag.getCompleteEndIndex() != endValue) ||
        (tag.getCompleteBeginIndex() < beginValue)) {
      return false;
    }

    // Extract reference
    int refLength = tag.getCompleteEndIndex() - tag.getCompleteBeginIndex();
    suffix = value.substring(value.length() - refLength);
    value = value.substring(0, value.length() - refLength);
    endValue -= refLength;
    return true;
  }

  /**
   * @return True if bold/italic formatting was extracted.
   */
  private boolean extractBoldItalic() {
    // Check that the value starts and ends with bold/italic
    if (!value.startsWith("''") || !value.endsWith("''")) {
      return false;
    }
    int formattingBegin = ContentsUtil.moveIndexForwardWhileFound(value, 0, "'");
    if (formattingBegin >= value.length()) {
      return false;
    }
    int formattingEnd = ContentsUtil.moveIndexBackwardWhileFound(value, value.length() - 1, "'") + 1;
    if (formattingBegin != value.length() - formattingEnd) {
      return false;
    }

    // Check if replacement can be automatic
    String contents = analysis.getContents();
    int beginIndex = function.getBeginIndex();
    char before = StringUtils.isEmpty(prefix) ?
        ((beginIndex > 0) ? contents.charAt(beginIndex - 1) : ' ') :
        prefix.charAt(prefix.length() - 1);
    automatic &= (before != '\'');
    int endIndex = function.getEndIndex();
    char after = StringUtils.isEmpty(suffix) ?
        ((endIndex < contents.length()) ? contents.charAt(endIndex) : ' ') :
        suffix.charAt(0);
    automatic &= (after != '\'');

    // Change the suggestion
    prefix += value.substring(0, formattingBegin);
    suffix = value.substring(formattingEnd) + suffix;
    value = value.substring(formattingBegin, formattingEnd);
    beginValue += formattingBegin;
    endValue -= formattingBegin;
    return true;
  }

  /**
   * @return True if the text contained no digits at all.
   */
  private boolean extractNoDigitsText() {
    // Check if the text contains no digits
    if (value.isEmpty()) {
      return false;
    }
    int lastIndex = ContentsUtil.moveIndexForwardWhileNotFound(value, 0, "0123456789");
    if (lastIndex < value.length()) {
      return false;
    }

    // Extract all the text
    prefix += value;
    beginValue += value.length();
    value = StringUtils.EMPTY;
    return true;
  }

  /**
   * Analyze if a formatnum: has a valid format.
   * 
   * @param analysis Page analysis
   * @param function formatnum: function.
   * @return True if the function has a valid format.
   */
  public static boolean isValidFormatnum(PageAnalysis analysis, PageElementFunction function) {
    // Check argument existence
    if (function.getParameterCount() < 1) {
      return false;
    }
    if (function.getParameterCount() > 1) {
      return true;
    }
    return isValidFormatnum(
        analysis,
        function.getParameterValueNotTrimmed(0),
        function.getParameterValueNotTrimmedOffset(0));
  }

  /**
   * Analyze if a formatnum: argument has a valid format.
   * 
   * @param analysis Page analysis
   * @param argument formatnum: argument.
   * @return True if the argument has a valid format.
   */
  public static boolean isValidFormatnum(
      PageAnalysis analysis,
      String argument,
      int argumentBeginIndex) {
    // Check optional sign
    int tmpIndex = moveIndexForwardWhileFound(
        analysis, argumentBeginIndex, argument, 0, " \n", true);
    if (tmpIndex >= argument.length()) {
      return true;
    }
    if ("+-".indexOf(argument.charAt(tmpIndex)) >= 0) {
      tmpIndex++;
    }

    // Check digits before comma
    int previousIndex = tmpIndex;
    tmpIndex = moveIndexForwardWhileFound(
        analysis, argumentBeginIndex, argument, tmpIndex, "0123456789", false);
    boolean digits = (tmpIndex > previousIndex);

    // Check optional comma and digits after
    if ((tmpIndex < argument.length()) && (argument.charAt(tmpIndex) == '.')) {
      tmpIndex = moveIndexForwardWhileFound(analysis, argumentBeginIndex, argument, tmpIndex, ".", false);
      previousIndex = tmpIndex;
      tmpIndex = moveIndexForwardWhileFound(
          analysis, argumentBeginIndex, argument, tmpIndex, "0123456789", false);
      digits |= (tmpIndex > previousIndex);
    }

    // Check special cases with templates or functions
    while (tmpIndex < argument.length()) {
      final char currentChar = argument.charAt(tmpIndex);
      int charIndex = argumentBeginIndex + tmpIndex;
      if (currentChar == '{') {
        PageElementTemplate template = analysis.isInTemplate(charIndex);
        if ((template != null) && (template.getBeginIndex() == charIndex)) {
          tmpIndex += template.getEndIndex() - template.getBeginIndex();
          digits = true;
        } else {
          PageElementFunction function = analysis.isInFunction(charIndex);
          if ((function != null) && (function.getBeginIndex() == charIndex)) {
            tmpIndex += function.getEndIndex() - function.getBeginIndex();
            digits = true;
          } else {
            return false;
          }
        }
      } else if (" \n".indexOf(currentChar) >= 0) {
        tmpIndex++;
      } else {
        return false;
      }
    }

    // Check that we indeed have digits
    if (!digits) {
      return false;
    }

    // Check that we are at the end
    tmpIndex = moveIndexForwardWhileFound(
        analysis, argumentBeginIndex, argument, tmpIndex, " \n", true);
    if (tmpIndex < argument.length()) {
      return false;
    }

    return true;
  }

  /**
   * Move forward the index in the text while the character at the index is from the given set.
   * It also handles comments.
   * 
   * @param analysis Page analysis.
   * @param offset Offset of the contents in the page.
   * @param contents Text to analyze.
   * @param startIndex Start index.
   * @param set Set of characters to look for.
   * @param commentAtStart True if comments at the beginning should be accepted.
   * @return Minimum value of index (≥ startIndex) with a character not from the given set.
   *         If all characters after startIndex are from the given set, returns contents.length().
   */
  private static int moveIndexForwardWhileFound(
      PageAnalysis analysis,
      int offset,
      String contents,
      int startIndex,
      String set,
      boolean commentAtStart) {
    int tmpIndex = ContentsUtil.moveIndexForwardWhileFound(contents, startIndex, set);
    boolean commentFound = false;
    if ((tmpIndex > startIndex) || commentAtStart) {
      if ((tmpIndex < contents.length()) && (contents.charAt(tmpIndex) == '<')) {
        ContentsComment comment = analysis.comments().getBeginsAt(offset + tmpIndex);
        if (comment != null) {
          commentFound = true;
          tmpIndex += comment.getEndIndex() - comment.getBeginIndex();
        }
      }
    }
    if (commentFound) {
      tmpIndex = moveIndexForwardWhileFound(analysis, offset, contents, tmpIndex, set, true);
    }
    return tmpIndex;
  }
}
