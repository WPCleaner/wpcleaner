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
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.SimpleAction;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.data.CharacterUtils;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.contents.IntervalComparator;
import org.wikipediacleaner.gui.swing.action.ActionExternalViewer;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 544 of check wikipedia project.
 * Error 544: Missing end model of a pair.
 */
public class CheckErrorAlgorithm544 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm544() {
    super("Missing end model of a pair");
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

    // Check configuration
    if (pairs.isEmpty()) {
      return false;
    }
    
    // Analyze each pair
    boolean result = false;
    for (String[] pair : pairs) {
      result |= analyzePair(analysis, errors, pair);
    }

    return result;
  }

  private boolean analyzePair(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      String[] pair) {
    if ((pair == null) || (pair.length < 2)) {
      return false;
    }
    
    // Retrieve open templates
    List<PageElementTemplate> openTemplates = analysis.getTemplates(pair[0]);
    if ((openTemplates == null) || (openTemplates.isEmpty())) {
      return false;
    }
    
    // Retrieve close templates
    List<PageElementTemplate> closeTemplates = new ArrayList<>();
    for (int i = 1; i < pair.length; i++) {
      String closeTemplate = pair[i];
      List<PageElementTemplate> tmpTemplates = analysis.getTemplates(closeTemplate);
      if (tmpTemplates != null) {
        closeTemplates.addAll(tmpTemplates);
      }
    }
    closeTemplates.sort(new IntervalComparator());
    
    // Match templates together
    for (int i = openTemplates.size(); i > 0; i--) {
      PageElementTemplate openTemplate = openTemplates.get(i - 1);
      PageElementTemplate closeTemplate = closeTemplates.isEmpty() ? null : closeTemplates.get(closeTemplates.size() - 1);
      if ((closeTemplate != null) &&
          (closeTemplate.getBeginIndex() >= openTemplate.getEndIndex())) {
        closeTemplates.remove(closeTemplates.size() - 1);
        openTemplates.remove(i - 1);
      }
    }
    if (openTemplates.isEmpty()) {
      return false;
    }

    // Report errors
    if (errors == null) {
      return true;
    }
    EnumWikipedia wiki = analysis.getWikipedia();
    for (PageElementTemplate openTemplate : openTemplates) {
      int beginIndex = openTemplate.getBeginIndex();
      int endIndex = openTemplate.getEndIndex();
      CheckErrorResult errorResult = createCheckErrorResult(
          analysis, beginIndex, endIndex);
      errors.add(errorResult);

      // Check if the open template is just after a tag of some sort
      String contents = analysis.getContents();
      while ((beginIndex > 0) &&
             ((contents.charAt(beginIndex - 1) == '\n') ||
              (CharacterUtils.isWhitespace(contents.charAt(beginIndex - 1))))) {
        beginIndex--;
      }
      if ((beginIndex >0) && (contents.charAt(beginIndex - 1) == '>')) {
        PageElementTag tag = analysis.isInTag(beginIndex - 1);
        PageElementTag endTag = null;
        if ((tag != null) && tag.isComplete() && !tag.isFullTag() && !tag.isEndTag()) {
          if (PageElementTag.TAG_HTML_CENTER.equals(tag.getNormalizedName())) {
            endTag = tag.getMatchingTag();
          }
        }
        if (endTag != null) {
          beginIndex = endTag.getBeginIndex();
          endIndex = endTag.getEndIndex();
          boolean newLine = (contents.charAt(beginIndex - 1) == '\n');
          errorResult = createCheckErrorResult(analysis, beginIndex, endIndex, ErrorLevel.WARNING);
          StringBuilder replacement = new StringBuilder();
          replacement.append(PageElementTemplate.createTemplate(pair[1]));
          if (newLine) {
            replacement.append('\n');
          }
          replacement.append(contents.substring(beginIndex, endIndex));
          errorResult.addReplacement(replacement.toString());
          errors.add(errorResult);
        }
      }

      // General actions
      errorResult.addPossibleAction(new SimpleAction(
          GT._T("External Viewer"),
          new ActionExternalViewer(
              wiki,
              wiki.getWikiConfiguration().getNamespace(Namespace.TEMPLATE).getCanonicalTitle() + ":" + openTemplate.getTemplateName())));
    }
    return true;
  }

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** List of templates working in pair */
  private static final String PARAMETER_PAIR_TEMPLATES = "pair_templates";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    String tmp = getSpecificProperty(PARAMETER_PAIR_TEMPLATES, true, true, false);
    pairs.clear();
    if (tmp != null) {
      List<String[]> tmpPairs = WPCConfiguration.convertPropertyToStringArrayList(tmp);
      if (tmpPairs != null) {
        pairs.addAll(tmpPairs);
      }
    }
  }

  /** Links to ignore */
  private final List<String[]> pairs = new ArrayList<>();

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_PAIR_TEMPLATES,
        GT._T("Pairs of templates: for each opening template, all possible closing templates")));
  }
}
