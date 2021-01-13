/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a0xx.a07x.a078;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.data.PageElement;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.IntervalComparator;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 78 of check wikipedia project.
 * Error 78: Reference double
 */
public class CheckErrorAlgorithm078 extends CheckErrorAlgorithmBase {

  /** Logs */
  private final Logger log = LoggerFactory.getLogger(CheckErrorAlgorithm078.class);

  public CheckErrorAlgorithm078() {
    super("Reference double");
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
    if (!analysis.getPage().isArticle()) {
      return false;
    }

    // Build a map of references by group name
    Map<String, List<PageElement>> referencesByGroup = new HashMap<>();

    // Manage <references> tags
    List<PageElementTag> referencesTags = analysis.getTags(WikiTagType.REFERENCES);
    if ((referencesTags != null) && !referencesTags.isEmpty()) {
      for (PageElementTag referencesTag : referencesTags) {
        
        // Use only beginning tags
        if (referencesTag.isFullTag() || !referencesTag.isEndTag()) {
          
          // Retrieve "group"
          PageElementTag.Parameter group = referencesTag.getParameter("group");
          String groupName = "";
          if ((group != null) && (group.getValue() != null)) {
            groupName = group.getValue();
          }

          // Store <references> tag
          List<PageElement> existingReferences = referencesByGroup.get(groupName);
          if (existingReferences == null) {
            existingReferences = new ArrayList<>();
            referencesByGroup.put(groupName, existingReferences);
          }
          existingReferences.add(referencesTag);
        }
      }
    }

    // Manage templates
    String contents = analysis.getContents();
    for (Pattern pattern : patterns) {
      Matcher matcher = pattern.matcher(contents);
      while (matcher.find()) {
        int beginIndex = matcher.start();
        PageElementTemplate template = analysis.isInTemplate(beginIndex);
        if (template != null) {
          String groupName = "";
          List<PageElement> existingReferences = referencesByGroup.get(groupName);
          if (existingReferences == null) {
            existingReferences = new ArrayList<>();
            referencesByGroup.put(groupName, existingReferences);
          }
          existingReferences.add(template);
        }
      }
    }

    // Report errors
    boolean result = false;
    for (List<PageElement> referencesForGroup : referencesByGroup.values()) {
      if ((referencesForGroup != null) && (referencesForGroup.size() > 1)) {
        if (errors == null) {
          return true;
        }
        result = true;

        // Find first suggested references for the group
        Collections.sort(referencesForGroup, new IntervalComparator());
        PageElement suggestedReferences = referencesForGroup.get(referencesForGroup.size() - 1);

        // Report errors
        for (PageElement referencesElement : referencesForGroup) {
          int beginIndex = referencesElement.getBeginIndex();
          int endIndex = referencesElement.getEndIndex();
          if (referencesElement instanceof PageElementTag) {
            PageElementTag tag = (PageElementTag) referencesElement;
            beginIndex = tag.getCompleteBeginIndex();
            endIndex = tag.getCompleteEndIndex();
          }
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, beginIndex, endIndex,
              (referencesElement == suggestedReferences) ? ErrorLevel.WARNING : ErrorLevel.ERROR);
          errorResult.addReplacement("");
          errors.add(errorResult);
        }
      }
    }

    return result;
  }

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** Regular expression to find templates replacing tags */
  private static final String PARAMETER_TEMPLATES = "templates";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    String tmp = getSpecificProperty(PARAMETER_TEMPLATES, true, true, false);
    patterns.clear();
    if (tmp != null) {
      List<String> tmpList = WPCConfiguration.convertPropertyToStringList(tmp);
      if (tmpList != null) {
        for (String templateRegexp : tmpList) {
          String patternText = null;
          try {
            if (templateRegexp.length() > 0) {
              char firstLetter = templateRegexp.charAt(0);
              if (Character.isUpperCase(firstLetter) || Character.isLowerCase(firstLetter)) {
                templateRegexp =
                    "[" + Character.toUpperCase(firstLetter) + Character.toLowerCase(firstLetter) + "]" +
                    templateRegexp.substring(1);
              }
            }
            patternText = "\\{\\{" + templateRegexp;
            patterns.add(Pattern.compile(patternText));
          } catch (PatternSyntaxException e) {
            log.warn(e.getMessage() + " (" + patternText + ")");
          }
        }
      }
    }
  }

  /** Links to ignore */
  private final List<Pattern> patterns = new ArrayList<>();

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_TEMPLATES,
        GT._T("A list of regular expressions to find templates replacing <references> tags"),
        new AlgorithmParameterElement(
            "regular expression",
            GT._T("A regular expression to find templates replacing <references> tags")),
        true));
  }
}
