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

import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.SpecialCharacters;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementCategory;
import org.wikipediacleaner.api.data.PageElementFunction;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.PageElementTitle;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 37 of check wikipedia project.
 * Error 37: Title with special letters and no DEFAULTSORT
 */
public class CheckErrorAlgorithm037 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm037() {
    super("Title with special letters and no DEFAULTSORT");
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
    if (analysis == null) {
      return false;
    }

    // Analyzing title to find special characters
    String title = analysis.getPage().getTitle();
    EnumWikipedia wiki = analysis.getWikipedia();
    boolean characterFound = false;
    boolean replaceable = true;
    int currentPos = 0;
    while ((currentPos < title.length()) && (currentPos < limit)) {
      char character = title.charAt(currentPos);
      if (!SpecialCharacters.isAuthorized(character, wiki)) {
        characterFound = true;
        String replacement = SpecialCharacters.proposeReplacement(character, wiki);
        for (int i = 0; i < replacement.length(); i++) {
          if (!SpecialCharacters.isAuthorized(replacement.charAt(i), wiki)) {
            replaceable = false;
          }
        }
      }
      currentPos++;
    }
    if (!characterFound) {
      return false;
    }

    // Searching a DEFAULTSORT tag
    List<PageElementFunction> defaultSorts = analysis.getDefaultSorts();
    if ((defaultSorts != null) && (defaultSorts.size() > 0)) {
      return false;
    }

    // Searching for Categories without a sort key
    boolean categoriesWithoutSort = false;
    List<PageElementCategory> categories = analysis.getCategories();
    if ((categories == null) || (categories.isEmpty())) {
      return false;
    }
    for (PageElementCategory category : categories) {
      if ((category.getSort() == null) ||
          (category.getSortNotTrimmed().length() == 0)) {
        categoriesWithoutSort = true;
      }
    }
    if (!categoriesWithoutSort) {
      return false;
    }

    // Reporting error
    if (errors == null) {
      return true;
    }
    PageElementCategory category = categories.get(0);
    int beginIndex = category.getBeginIndex();
    int endIndex = category.getEndIndex();
    String contents = analysis.getContents();
    String replacement =
        createDefaultSort(analysis) + "\n" +
        contents.substring(beginIndex, endIndex);
    boolean automatic = replaceable;
    if (automatic) {
      int index = beginIndex;
      while ((index > 0) && (contents.charAt(index - 1) == ' ')) {
        index--;
      }
      if ((index > 0) && (contents.charAt(index - 1) != '\n')) {
        automatic = false;
      }
      if (analysis.getPage().getRedirects().isRedirect() && !automatic) {
        automatic = true;
        replacement = "\n\n" + replacement;
      }
    }
    ErrorLevel errorLevel = ErrorLevel.ERROR;
    if (automatic) {
      String templates = getSpecificProperty("templates", true, false, false);
      if (templates != null) {
        for (String template : templatesList) {
          List<PageElementTemplate> foundTemplates = analysis.getTemplates(template);
          if ((foundTemplates != null) && (foundTemplates.size() > 0)) {
            automatic = false;
            errorLevel = ErrorLevel.WARNING;
          }
        }
      }
    }
    if (automatic) {
      int index = endIndex;
      while ((index < contents.length()) && (contents.charAt(index) == ' ')) {
        index++;
      }
      if ((index < contents.length()) && (contents.charAt(index) != '\n')) {
        automatic = false;
      }
    }
    if (automatic) {
      List<PageElementTitle> titles = analysis.getTitles();
      if ((titles != null) && (titles.size() > 0)) {
        if (beginIndex < titles.get(titles.size() - 1).getEndIndex()) {
          automatic = false;
        }
      }
    }
    if (automatic) {
      if (!CheckErrorAlgorithms.isAlgorithmActive(wiki, 6)) {
        automatic = false;
      }
    }

    CheckErrorResult errorResult = createCheckErrorResult(
        analysis, beginIndex, endIndex, errorLevel);
    errorResult.addReplacement(replacement, GT._T("Add DEFAULTSORT"), automatic);
    errors.add(errorResult);
    return true;
  }

  /**
   * Automatic fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalAutomaticFix(PageAnalysis analysis) {
    return fixUsingAutomaticReplacement(analysis);
  }

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** Number of characters to check */
  private static final String PARAMETER_FIRST_CHARACTERS = "first_characters";

  /** Templates that prevent automatic fixing */
  private static final String PARAMETER_TEMPLATES = "templates";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    String tmp = getSpecificProperty(PARAMETER_FIRST_CHARACTERS, true, false, false);
    limit = 5;
    if (tmp != null) {
      try {
        limit = Integer.valueOf(tmp);
      } catch (NumberFormatException e) {
        //
      }
    }

    tmp = getSpecificProperty("templates", true, false, false);
    templatesList.clear();
    if (tmp != null) {
      List<String> tmpList = WPCConfiguration.convertPropertyToStringList(tmp);
      if (tmpList != null) {
        templatesList.addAll(tmpList);
      }
    }
  }

  /** Number of characters to check */
  private int limit = 5;

  /** Templates that prevent automatic fixing */
  private static final List<String> templatesList = new ArrayList<>();

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_FIRST_CHARACTERS,
        GT._T("Restrict the detection to the first characters"),
        new AlgorithmParameterElement(
            "number",
            GT._T("Maximum number of characters to use for the detection"))));
    addParameter(new AlgorithmParameter(
        PARAMETER_TEMPLATES,
        GT._T("List of templates that prevent automatic fixing of this error"),
        new AlgorithmParameterElement(
            "template name",
            GT._T("Template that prevent automatic fixing of this error")),
        true));
  }
}
