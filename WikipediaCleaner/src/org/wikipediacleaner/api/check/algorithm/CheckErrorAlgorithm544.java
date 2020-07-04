/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.SimpleAction;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.data.CharacterUtils;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
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
    if (pairsMap.isEmpty()) {
      return false;
    }
    
    // Analyze each template
    List<PageElementTemplate> templates = analysis.getTemplates();
    if ((templates == null) || (templates.isEmpty())) {
      return false;
    }
    LinkedList<Pair<PageElementTemplate, PairInformation>> openingTemplates = new LinkedList<>();
    for (PageElementTemplate template : templates) {
      String templateName = Page.normalizeTitle(template.getTemplateName());

      // Check if it's an opening template, and memorize it
      PairInformation pairInfo = pairsMap.get(templateName);
      if (pairInfo != null) {
        openingTemplates.addLast(new ImmutablePair<>(template, pairInfo));
      }

      // Check if it's a closing template, and pair it with the opening template
      if (!openingTemplates.isEmpty()) {
        boolean match = false;
        Iterator<Pair<PageElementTemplate, PairInformation>> itOpeningTemplates = openingTemplates.descendingIterator();
        while (!match && itOpeningTemplates.hasNext()) {
          Pair<PageElementTemplate, PairInformation> openingTemplate = itOpeningTemplates.next();
          if (openingTemplate.getRight().isACloseTemplate(templateName)) {
            match = true;
            itOpeningTemplates.remove();
          }
        }
      }
    }

    // Report errors
    if (openingTemplates.isEmpty()) {
      return false;
    }
    for (Pair<PageElementTemplate, PairInformation> openingTemplate : openingTemplates) {
      reportError(analysis, errors, openingTemplate.getLeft(), openingTemplate.getRight().getReplacement());
    }

    return true;
  }

  /**
   * Report an error for an opening template.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param openTemplate Opening template for which the error should be reported.
   * @param closeTemplate Name of a closing template.
   */
  private void reportError(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      PageElementTemplate openTemplate,
      String closeTemplate) {
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
        replacement.append(PageElementTemplate.createTemplate(closeTemplate));
        if (newLine) {
          replacement.append('\n');
        }
        replacement.append(contents.substring(beginIndex, endIndex));
        errorResult.addReplacement(replacement.toString());
        errors.add(errorResult);
      }
    }

    // General actions
    EnumWikipedia wiki = analysis.getWikipedia();
    errorResult.addPossibleAction(new SimpleAction(
        GT._T("External Viewer"),
        new ActionExternalViewer(
            wiki,
            wiki.getWikiConfiguration().getNamespace(Namespace.TEMPLATE).getCanonicalTitle() + ":" + openTemplate.getTemplateName())));
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
    pairsMap.clear();
    if (tmp != null) {
      List<String[]> tmpPairs = WPCConfiguration.convertPropertyToStringArrayList(tmp);
      if (tmpPairs != null) {
        for (String[] tmpPair : tmpPairs) {
          if (tmpPair.length > 1) {
            String openTemplateName = Page.normalizeTitle(tmpPair[0]);
            PairInformation pairInfo = pairsMap.get(openTemplateName);
            if (pairInfo == null) {
              pairInfo = new PairInformation();
              pairsMap.put(openTemplateName, pairInfo);
            }
            pairInfo.addCloseTemplates(tmpPair);
          }
        }
      }
    }
  }

  /** Links to ignore */
  private final Map<String, PairInformation> pairsMap = new HashMap<>();

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_PAIR_TEMPLATES,
        GT._T("Pairs of templates: for each opening template, all possible closing templates"),
        new AlgorithmParameterElement[] {
            new AlgorithmParameterElement(
                "opening template name",
                GT._T("Name of the opening template in the pair")),
            new AlgorithmParameterElement(
                "closing template name",
                GT._T("Name of a closing template in the pair"),
                false, true)
        },
        true));
  }

  /**
   * Bean for holding information about a pair of templates.
   */
  private static class PairInformation {
    private Set<String> closeTemplates;
    private String replacement;

    public PairInformation() {
      this.closeTemplates = new HashSet<>();
    }

    public void addCloseTemplates(String[] close) {
      for (int index = 1; index < close.length; index++) {
        if (replacement == null) {
          replacement = close[index];
        }
        closeTemplates.add(close[index]);
      }
    }

    public String getReplacement() {
      return replacement;
    }

    public boolean isACloseTemplate(String templateName) {
      return closeTemplates.contains(Page.normalizeTitle(templateName));
    }
  }
}
