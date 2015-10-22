/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.check.AddTextActionProvider;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.constants.WikiConfiguration;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.StringChecker;
import org.wikipediacleaner.utils.StringCheckerUnauthorizedCharacters;


/**
 * Algorithm for analyzing error 090 of check wikipedia project.
 * Error 090: Internal link written as external link
 */
public class CheckErrorAlgorithm090 extends CheckErrorAlgorithmBase {

  /**
   * String checker for text inputed by user.
   */
  private final StringChecker checker;

  /**
   * Possible global fixes.
   */
  private final static String[] globalFixes = new String[] {
    GT._("Convert them to internal links"),
  };

  public CheckErrorAlgorithm090() {
    super("Internal link written as external link");
    checker = new StringCheckerUnauthorizedCharacters("[]\"");
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

    // Analyze each link
    boolean result = false;
    result |= analyzeExternalLinks(analysis, errors, onlyAutomatic);
    result |= analyzeInternalLinks(analysis, errors, onlyAutomatic);

    return result;
  }

  /**
   * Analyze external links.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param onlyAutomatic True if analysis could be restricted to errors automatically fixed.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeExternalLinks(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors, boolean onlyAutomatic) {

    // Configuration
    String templates = getSpecificProperty("link_templates", true, true, false);
    List<String> linkTemplates = null;
    if (templates != null) {
      linkTemplates = WPCConfiguration.convertPropertyToStringList(templates);
    }

    // Analyze each external link
    boolean result = false;
    List<PageElementExternalLink> links = analysis.getExternalLinks();
    if (links == null) {
      return result;
    }
    EnumWikipedia wiki = analysis.getWikipedia();
    WikiConfiguration wikiConf = wiki.getWikiConfiguration();
    String contents = analysis.getContents();
    for (PageElementExternalLink link : links) {
      String article = wikiConf.isArticleUrl(link.getLink());
      if ((article != null) &&
          (article.length() > 0)) {
        if (errors == null) {
          return true;
        }
        result = true;
        int beginIndex = link.getBeginIndex();
        int endIndex = link.getEndIndex();
        if (link.hasSquare()) {
          if ((beginIndex > 0) && (contents.charAt(beginIndex - 1) == '[') &&
              (endIndex < contents.length()) && (contents.charAt(endIndex) == ']')) {
            beginIndex--;
            endIndex++;
          }
        }

        String text = link.getText();

        // Check if link is in template
        if (linkTemplates != null) {
          PageElementTemplate template = analysis.isInTemplate(beginIndex);
          if (template != null) {
            for (String linkTemplate : linkTemplates) {
              String[] elements = linkTemplate.split("\\|");
              if ((elements.length > 2) &&
                  Page.areSameTitle(elements[0], template.getTemplateName()) &&
                  link.getLink().trim().equals(template.getParameterValue(elements[1]))) {
                text = template.getParameterValue(elements[2]);
                beginIndex = template.getBeginIndex();
                endIndex = template.getEndIndex();
              }
            }
          }
        }

        CheckErrorResult errorResult = createCheckErrorResult(
            analysis, beginIndex, endIndex);
        if (link.getLink().indexOf('?') < 0) {
          Page articlePage = DataManager.getPage(analysis.getWikipedia(), article, null, null, null);
          boolean needColon = false;
          if (articlePage.getNamespace() != null) {
            int ns = articlePage.getNamespace().intValue();
            if (ns % 2 == 0) {
              if ((ns != Namespace.MAIN) &&
                  (ns != Namespace.USER) &&
                  (ns != Namespace.HELP) &&
                  (ns != Namespace.MEDIAWIKI) &&
                  (ns != Namespace.TEMPLATE) &&
                  (ns != Namespace.WIKIPEDIA)) {
                needColon = true;
              }
            }
          }
          if (text != null) {
            errorResult.addReplacement(
                PageElementInternalLink.createInternalLink(
                    (needColon ? ":" : "") + article, text),
                true);
          } else {
            String question = GT._("What text should be displayed by the link?");
            AddTextActionProvider action = new AddTextActionProvider(
                "[[" + (needColon ? ":" : "") + article + "|", "]]", null,
                question, article, checker);
            errorResult.addPossibleAction(
                GT._("Convert into an internal link"),
                action);
          }
        }
        errors.add(errorResult);
      }
    }
    return result;
  }

  /**
   * Analyze internal links.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param onlyAutomatic True if analysis could be restricted to errors automatically fixed.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeInternalLinks(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors, boolean onlyAutomatic) {
    boolean result = false;
    List<PageElementInternalLink> links = analysis.getInternalLinks();
    if (links == null) {
      return result;
    }
    EnumWikipedia wiki = analysis.getWikipedia();
    String host = wiki.getSettings().getHost();
    for (PageElementInternalLink link : links) {
      String target = link.getLink();
      if ((target != null) && (target.length() >= host.length()) &&
          Page.areSameTitle(host, target.substring(0, host.length()))) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis, link.getBeginIndex(), link.getEndIndex());
        errors.add(errorResult);
      }
    }
    return result;
  }

  /**
   * Bot fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalBotFix(PageAnalysis analysis) {
    return fix(globalFixes[0], analysis, null);
  }

  /**
   * @return List of possible global fixes.
   */
  @Override
  public String[] getGlobalFixes() {
    return globalFixes;
  }

  /**
   * Fix all the errors in the page.
   * 
   * @param fixName Fix name (extracted from getGlobalFixes()).
   * @param analysis Page analysis.
   * @param textPane Text pane.
   * @return Page contents after fix.
   */
  @Override
  public String fix(String fixName, PageAnalysis analysis, MWPane textPane) {
    return fixUsingAutomaticReplacement(analysis);
  }

  /**
   * @return Map of parameters (Name -> description).
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#getParameters()
   */
  @Override
  public Map<String, String> getParameters() {
    Map<String, String> parameters = super.getParameters();
    parameters.put("link_templates", GT._("Templates using external links"));
    return parameters;
  }
}
