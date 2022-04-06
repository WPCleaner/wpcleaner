/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2021  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm.a5xx.a56x.a567;

import org.wikipediacleaner.api.check.HtmlCharacters;
import org.wikipediacleaner.api.data.PageElementFunction;
import org.wikipediacleaner.api.data.PageElementParameter;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.ContentsUtil;
import org.wikipediacleaner.api.data.contents.comment.ContentsComment;
import org.wikipediacleaner.api.data.contents.magicword.FunctionMagicWordType;

/**
 * Manage numeric values.
 */
public class Numeric {

  protected final PageAnalysis analysis;
  protected final int initialBeginValue;
  protected final int initialEndValue;

  protected int beginValue;
  protected int endValue;
  protected String value;
  protected boolean automatic;

  public Numeric(
      final PageAnalysis analysis,
      final int initialBeginValue,
      final int initialEndValue,
      final String value) {
    this.analysis = analysis;
    this.initialBeginValue = initialBeginValue;
    this.initialEndValue = initialEndValue;
    this.beginValue = initialBeginValue;
    this.endValue = initialEndValue;
    this.value = value;
    this.automatic = true;
  }

  protected static final String WHITESPACE =
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
  protected boolean removeWhitespaceBetweenDigits() {
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
   * @param minSeparators Minimum number of separators to remove them.
   * @return True if thousand separators were removed.
   */
  protected boolean removeThousandSeparators(int minSeparators) {
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
    int separatorCount = 0;
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
      separatorCount++;
    }
    if (endDigit < value.length()) {
      return false;
    }
    if (separatorCount < minSeparators) {
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
   * @return True if commas were removed after the dot.
   */
  protected boolean removeCommasAfterDot() {
    if (value.isEmpty()) {
      return false;
    }
    int beginDigit = ContentsUtil.moveIndexForwardWhileFound(value, 0, "+-");
    int endDigit = ContentsUtil.moveIndexForwardWhileFound(value, beginDigit, "0123456789");
    if ((endDigit == beginDigit) || (endDigit >= value.length())) {
      return false;
    }
    if (value.charAt(endDigit) != '.') {
      return false;
    }
    beginDigit = endDigit + 1;
    StringBuilder tmp = new StringBuilder(value.substring(0, beginDigit));
    int commaCount = 0;
    while (beginDigit < value.length()) {
      endDigit = ContentsUtil.moveIndexForwardWhileFound(value, beginDigit, "0123456789");
      if (endDigit == beginDigit) {
        return false;
      }
      tmp.append(value.substring(beginDigit, endDigit));
      if (endDigit == value.length()) {
        if (commaCount > 0) {
          value = tmp.toString();
          return true;
        }
        return false;
      }
      if (value.charAt(endDigit) != ',') {
        return false;
      }
      commaCount++;
      beginDigit = endDigit + 1;
    }
    return false;
  }

  /**
   * @return True if the comma has been replaced by a dot.
   */
  protected boolean replaceCommaByDot() {
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
   * @return True if incorrect minus sign was replaced.
   */
  protected boolean replaceIncorrectMinus() {
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
   * @return True if a formatnum was extracted from the text.
   */
  protected boolean removeFormatnum() {
    if (value.isEmpty()) {
      return false;
    }

    // Check if the value is a formatnum
    char firstChar = value.charAt(0);
    char lastChar = value.charAt(value.length() - 1);
    if ((firstChar != '{') || (lastChar != '}')) {
      return false;
    }
    PageElementFunction function = analysis.isInFunction(beginValue);
    if ((function == null) ||
        (function.getBeginIndex() != beginValue) ||
        (function.getEndIndex() != endValue) ||
        (function.getMagicWord() == null) ||
        !FunctionMagicWordType.FORMAT_NUM.equals(function.getMagicWord().getType()) ||
        (function.getParameterCount() != 1)) {
      return false;
    }

    // Extract value
    value = function.getParameterValueNotTrimmed(0);
    beginValue += function.getParameterValueNotTrimmedOffset(0) - function.getBeginIndex();
    endValue -= function.getEndIndex() - function.getParameterValueNotTrimmedOffset(0) - value.length();
    return true;
  }

  /**
   * Analyze if a text is a valid numeric value.
   * 
   * @param analysis Page analysis
   * @param argument Text.
   * @param argumentBeginIndex Index of the beginning of the argument.
   * @return True if the argument has a valid format.
   */
  public static boolean isValidFormatnum(
      PageAnalysis analysis,
      String argument,
      int argumentBeginIndex) {
    // Check optional sign
    int tmpIndex = moveIndexForwardWhileFound(
        analysis, argumentBeginIndex, argument, 0, " \n", true, false);
    if (tmpIndex >= argument.length()) {
      return true;
    }
    if ("+-".indexOf(argument.charAt(tmpIndex)) >= 0) {
      tmpIndex++;
    }

    // Check digits before comma
    int previousIndex = tmpIndex;
    tmpIndex = moveIndexForwardWhileFound(
        analysis, argumentBeginIndex, argument, tmpIndex, "0123456789", false, true);
    boolean digits = (tmpIndex > previousIndex);

    // Check optional comma and digits after
    if ((tmpIndex < argument.length()) && (argument.charAt(tmpIndex) == '.')) {
      tmpIndex = moveIndexForwardWhileFound(analysis, argumentBeginIndex, argument, tmpIndex, ".", false, false);
      previousIndex = tmpIndex;
      tmpIndex = moveIndexForwardWhileFound(
          analysis, argumentBeginIndex, argument, tmpIndex, "0123456789", false, true);
      digits |= (tmpIndex > previousIndex);
    }

    // Check that we indeed have digits
    if (!digits) {
      return false;
    }

    // Check that we are at the end
    tmpIndex = moveIndexForwardWhileFound(
        analysis, argumentBeginIndex, argument, tmpIndex, " \n", true, false);
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
   * @param otherStuff True if other constructions should be taken into account.
   * @return Minimum value of index (≥ startIndex) with a character not from the given set.
   *         If all characters after startIndex are from the given set, returns contents.length().
   */
  protected static int moveIndexForwardWhileFound(
      PageAnalysis analysis,
      int offset,
      String contents,
      int startIndex,
      String set,
      boolean commentAtStart,
      boolean otherStuff) {
    // Move forward while the character is from the given set
    int tmpIndex = ContentsUtil.moveIndexForwardWhileFound(contents, startIndex, set);

    // Check for comments
    boolean somethingFound = false;
    if ((tmpIndex > startIndex) || commentAtStart) {
      if ((tmpIndex < contents.length()) &&
          (contents.charAt(tmpIndex) == '<')) {
        ContentsComment comment = analysis.comments().getBeginsAt(offset + tmpIndex);
        if (comment != null) {
          somethingFound = true;
          tmpIndex += comment.getEndIndex() - comment.getBeginIndex();
        }
      }
    }

    // Check for other stuff
    if (otherStuff &&
        (tmpIndex < contents.length()) &&
        (contents.charAt(tmpIndex) == '{')) {
      PageElementTemplate template = analysis.isInTemplate(offset + tmpIndex);
      if ((template != null) &&
          (template.getBeginIndex() == offset + tmpIndex)) {
        somethingFound = true;
        tmpIndex += template.getEndIndex() - template.getBeginIndex();
      } else {
        PageElementFunction function = analysis.isInFunction(offset + tmpIndex);
        if ((function != null) &&
            (function.getBeginIndex() == offset + tmpIndex)) {
          if (!FunctionMagicWordType.FORMAT_NUM.equals(function.getMagicWord().getType()) ||
              (function.getParameterCount() > 1)) {
            somethingFound = true;
            tmpIndex += function.getEndIndex() - function.getBeginIndex();
          }
        } else {
          PageElementParameter parameter = analysis.isInParameter(offset + tmpIndex);
          if ((parameter != null) &&
              (parameter.getBeginIndex() == offset + tmpIndex)) {
            somethingFound = true;
            tmpIndex += parameter.getEndIndex() - parameter.getBeginIndex();
          }
        }
      }
    }

    // Recursive call if something was found
    if (somethingFound) {
      tmpIndex = moveIndexForwardWhileFound(analysis, offset, contents, tmpIndex, set, true, otherStuff);
    }
    return tmpIndex;
  }
}
