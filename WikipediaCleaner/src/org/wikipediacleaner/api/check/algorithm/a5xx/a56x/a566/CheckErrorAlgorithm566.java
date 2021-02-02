/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2021  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm.a5xx.a56x.a566;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import javax.annotation.Nonnull;

import org.apache.commons.lang3.StringUtils;
import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.SimpleAction;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmTags;
import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.tag.HtmlTagType;
import org.wikipediacleaner.api.data.contents.tag.TagType;
import org.wikipediacleaner.gui.swing.action.ActionExternalViewer;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.string.CharacterUtils;


/**
 * Algorithm for analyzing error 566 of check wikipedia project.
 * Error 566: abbr tags
 */
public class CheckErrorAlgorithm566 extends CheckErrorAlgorithmTags {

  private static final @Nonnull Set<TagType> tags = Collections.singleton(HtmlTagType.ABBR);

  private static final @Nonnull List<String> prefixes = Arrays.asList(
      "&nbsp;",
      " "
      );

  private static final @Nonnull List<String> suffixes = Arrays.asList(
      "&nbsp;",
      " "
      );

  public CheckErrorAlgorithm566() {
    super("<abbr> tags");

  }

  /**
   * @return Tags to look for.
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmTags#getTags()
   */
  @Override
  protected Set<TagType> getTags() {
    return tags;
  }

  /**
   * Report an error for one tag.
   * 
   * @param analysis Page analysis.
   * @param errors Errors.
   * @param tag Tag.
   */
  @Override
  protected void reportTag(
      @Nonnull PageAnalysis analysis,
      @Nonnull Collection<CheckErrorResult> errors,
      @Nonnull PageElementTag tag) {

    // Analyze area
    boolean reportComplete = reportCompleteTags();
    String contents = analysis.getContents();
    int beginTagIndex = reportComplete ? tag.getCompleteBeginIndex() : tag.getBeginIndex();
    int beginIndex = beginTagIndex;
    while ((beginIndex > 0) &&
        (CharacterUtils.isClassicLetter(contents.charAt(beginIndex - 1)) || Character.isDigit(contents.charAt(beginIndex - 1)))) {
      beginIndex--;
    }
    int endTagIndex = reportComplete ? tag.getCompleteEndIndex() : tag.getEndIndex();
    int endIndex = endTagIndex;
    while ((endIndex < contents.length()) &&
        (CharacterUtils.isClassicLetter(contents.charAt(endIndex)) || Character.isDigit(contents.charAt(endIndex)))) {
      endIndex++;
    }

    // Detect if automatic should be prevented
    boolean tmpAutomatic = true;
    for (int paramIndex = 0; paramIndex < tag.getParametersCount(); paramIndex++) {
      if (!StringUtils.equals("title", tag.getParameter(paramIndex).getName())) {
        tmpAutomatic = false;
      }
    }
    final boolean canAutomatic = tmpAutomatic;

    // Analyze prefix and suffix
    String tmpPrefix = contents.substring(beginIndex, beginTagIndex);
    String tmpSuffix = contents.substring(endTagIndex, endIndex);
    String tagContents = StringUtils.EMPTY;
    if (!tag.isFullTag()) {
      tagContents = contents.substring(tag.getValueBeginIndex(), tag.getValueEndIndex());
    }
    boolean found = false;
    do {
      found = false;
      for (String possiblePrefix : prefixes) {
        if (tagContents.startsWith(possiblePrefix)) {
          tmpPrefix += possiblePrefix;
          tagContents = tagContents.substring(possiblePrefix.length());
          found = true;
        }
      }
    } while (found);
    final String prefix = tmpPrefix;
    do {
      found = false;
      for (String possibleSuffix : suffixes) {
        if (tagContents.endsWith(possibleSuffix)) {
          tmpSuffix = possibleSuffix + tmpSuffix;
          tagContents = tagContents.substring(0, tagContents.length() - possibleSuffix.length());
          found = true;
        }
      }
    } while (found);
    final String suffix = tmpSuffix;

    // Report error
    CheckErrorResult errorResult = createCheckErrorResult(analysis, beginIndex, endIndex);
    if (tag.isFullTag()) {
      errorResult.addReplacement(prefix + suffix);
    } else {
      PageElementTag.Parameter paramTitle = tag.getParameter("title");
      String title = (paramTitle != null) ? paramTitle.getValue() : null;
      replacementConfig.getReplacement(errorResult, tagContents, title).ifPresent(
          r -> errorResult.addReplacement(prefix + r.replacement + suffix, r.automatic && canAutomatic));
      for (TemplateConfiguration template : templates) {
        template.getReplacement(errorResult, tagContents, title).ifPresent(
            r -> errorResult.addReplacement(prefix + r.replacement + suffix, r.automatic && canAutomatic));
      }
      errorResult.addReplacement(prefix + tagContents + suffix);
    }
    for (String category : categories) {
      String categoryName = "Category:" + category;
      errorResult.addPossibleAction(new SimpleAction(
          GT._T("External Viewer") + " - " + categoryName,
          new ActionExternalViewer(analysis.getWikipedia(), categoryName)));
    }
    errors.add(errorResult);
  }

  /**
   * Automatic fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalAutomaticFix(PageAnalysis analysis) {
    if (!analysis.getPage().isArticle() ||
        !analysis.getPage().isInMainNamespace()) {
      return analysis.getContents();
    }
    return fixUsingAutomaticReplacement(analysis);
  }

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** Categories for templates that can be used for an abbreviation */
  private static final String PARAMETER_CATEGORIES = "categories";

  /** Generic templates that can be used for an abbreviation */
  private static final String PARAMETER_TEMPLATES = "templates";

  /** Possible replacements based on abbreviation value */
  private static final String PARAMETER_REPLACEMENTS = "replacements";

  /** Possible replacements based on abbreviation value and title */
  private static final String PARAMETER_REPLACEMENTS_TITLE = "replacements_title";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    String tmp = getSpecificProperty(PARAMETER_TEMPLATES, true, true, false);
    templates.clear();
    if (tmp != null) {
      List<String[]> tmpList = WPCConfiguration.convertPropertyToStringArrayList(tmp);
      TemplateConfiguration.initializeConfiguration(templates, tmpList);
    }

    tmp = getSpecificProperty(PARAMETER_CATEGORIES, true, true, false);
    categories.clear();
    if (tmp != null) {
      List<String> tmpList = WPCConfiguration.convertPropertyToStringList(tmp, false);
      if (tmpList != null) {
        categories.addAll(tmpList);
      }
    }

    tmp = getSpecificProperty(PARAMETER_REPLACEMENTS, true, true, false);
    replacementConfig.clearConfiguration();
    if (tmp != null) {
      List<String[]> tmpList = WPCConfiguration.convertPropertyToStringArrayList(tmp);
      replacementConfig.setReplacementsByValue(tmpList);
    }
    tmp = getSpecificProperty(PARAMETER_REPLACEMENTS_TITLE, true,  true,  false);
    if (tmp != null) {
      List<String[]> tmpList = WPCConfiguration.convertPropertyToStringArrayList(tmp, 4);
      replacementConfig.setReplacementsByValueAndTitle(tmpList);
    }
  }

  /** Categories for templates that can be used for an abbreviation */
  private final List<String> categories = new ArrayList<>();

  /** Generic templates that can be used for an abbreviation */
  private final List<TemplateConfiguration> templates = new ArrayList<>();

  /** Possible replacements */
  private final ReplacementConfiguration replacementConfig = new ReplacementConfiguration();

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_REPLACEMENTS,
        GT._T("Possible replacements based on abbreviation value"),
        new AlgorithmParameterElement[] {
            new AlgorithmParameterElement(
                "value",
                GT._T("Value of the abbreviation")),
            new AlgorithmParameterElement(
                "true/false",
                GT._T("True if the replacement can be automatic")),
            new AlgorithmParameterElement(
                "replacement",
                GT._T("Replacement for the abbreviation"))
        },
        true));
    addParameter(new AlgorithmParameter(
        PARAMETER_REPLACEMENTS_TITLE,
        GT._T("Possible replacements based on abbreviation value and title"),
        new AlgorithmParameterElement[] {
            new AlgorithmParameterElement(
                "value",
                GT._T("Value of the abbreviation")),
            new AlgorithmParameterElement(
                "title",
                GT._T("Title of the abbreviation")),
            new AlgorithmParameterElement(
                "true/false",
                GT._T("True if the replacement can be automatic")),
            new AlgorithmParameterElement(
                "replacement",
                GT._T("Replacement for the abbreviation"))
        },
        true));
    addParameter(new AlgorithmParameter(
        PARAMETER_TEMPLATES,
        GT._T("Templates which can replace an abbreviation"),
        new AlgorithmParameterElement[] {
            new AlgorithmParameterElement(
                "template",
                GT._T("Name of the template")),
            new AlgorithmParameterElement(
                "abbreviation",
                GT._T("Name of the parameter for the abbreviation")),
            new AlgorithmParameterElement(
                "meaning",
                GT._T("Name of the parameter for the meaning")),
            new AlgorithmParameterElement(
                "default meaning",
                GT._T("Default value for the meaning"),
                true,
                false)
        },
        true));
  }
}
