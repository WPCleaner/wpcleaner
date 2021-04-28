/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2021  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm.a5xx.a56x.a568;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.a5xx.a56x.a567.Numeric;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;

/**
 * Manage numeric template parameters error detection and fixing
 */
class NumericTemplateParam extends Numeric {

  private final PageElementTemplate.Parameter parameter;
  private final boolean onlyInteger;

  public NumericTemplateParam(
      final PageAnalysis analysis,
      final PageElementTemplate.Parameter parameter,
      final Boolean onlyInteger) {
    super(
        analysis,
        parameter.getValueStartIndex(),
        parameter.getValueStartIndex() + parameter.getValue().length(),
        parameter.getValue());
    this.parameter = parameter;
    this.onlyInteger = Boolean.TRUE.equals(onlyInteger);
  }

  /**
   * Add suggestions for an invalid numeric template parameter
   * 
   * @param errorResult
   */
  public void addSuggestions(CheckErrorResult errorResult) {
    boolean tryAgain = true;
    while (tryAgain) {
      tryAgain = false;
      tryAgain |= replaceIncorrectMinus();
      tryAgain |= removeWhitespaceBetweenDigits();
      tryAgain |= removeThousandSeparators(onlyInteger ? 1 : 2);
      tryAgain |= removeCommasAfterDot();
      tryAgain |= replaceCommaByDot();
      tryAgain |= removeFormatnum();
    }

    // Replace if the new value is valid
    if (isValidFormatnum(analysis, value, beginValue)) {
      String contents = analysis.getContents();
      String prefix = contents.substring(parameter.getBeginIndex(), initialBeginValue);
      String suffix = contents.substring(initialEndValue, parameter.getEndIndex());
      errorResult.addReplacement(
          prefix + value + suffix,
          automatic);
      return;
    }
  }
}
