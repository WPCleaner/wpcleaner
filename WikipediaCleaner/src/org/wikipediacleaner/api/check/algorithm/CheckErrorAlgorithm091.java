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
import org.wikipediacleaner.api.check.CheckLanguageLinkActionProvider;
import org.wikipediacleaner.api.constants.CWConfigurationError;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.data.Interwiki;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.StringChecker;
import org.wikipediacleaner.utils.StringCheckerUnauthorizedCharacters;


/**
 * Algorithm for analyzing error 091 of check wikipedia project.
 * Error 091: Interwiki link written as external link
 */
public class CheckErrorAlgorithm091 extends CheckErrorAlgorithmBase {

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

  public CheckErrorAlgorithm091() {
    super("Interwiki link written as external link");
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
    if ((analysis == null) || (analysis.getInternalLinks() == null)) {
      return false;
    }

    // Retrieve configuration
    EnumWikipedia wiki = analysis.getWikipedia();
    CWConfigurationError error68 = wiki.getCWConfiguration().getErrorConfiguration(68);
    List<String> templatesList = null;
    if (error68 != null) {
      String templatesParam = error68.getSpecificProperty("template", true, false, false, false);
      if (templatesParam != null) {
        templatesList = WPCConfiguration.convertPropertyToStringList(templatesParam);
      }
    }
    String strOnlyLanguage = getSpecificProperty("only_language", true, false, false);
    boolean onlyLanguage = (strOnlyLanguage != null) ? Boolean.valueOf(strOnlyLanguage) : true;
    String strOnlyLocal = getSpecificProperty("only_local", true, false, false);
    boolean onlyLocal = (strOnlyLocal != null) ? Boolean.valueOf(strOnlyLocal) : true;

    // Analyze each external link
    boolean result = false;
    List<PageElementExternalLink> links = analysis.getExternalLinks();
    if (links == null) {
      return result;
    }
    List<Interwiki> interwikis = wiki.getWikiConfiguration().getInterwikis();
    String contents = analysis.getContents();
    for (PageElementExternalLink link : links) {
      // Check if this is a external link to an other wiki
      String article = null;
      String prefix = null;
      String language = null;
      boolean local = false;
      for (Interwiki interwiki : interwikis) {
        String tmp = interwiki.isArticleUrl(link.getLink());
        if (tmp != null) {
          if ((article == null) || (interwiki.getLanguage() != null)) {
            article = tmp;
            prefix = interwiki.getPrefix();
            language = interwiki.getLanguage();
            local = interwiki.getLocal();
          }
        }
      }
      EnumWikipedia fromWiki = null;
      if (prefix != null) {
        fromWiki = EnumWikipedia.getWikipedia(prefix);
        if (!prefix.equals(fromWiki.getSettings().getCode())) {
          fromWiki = null;
        }
      }

      // Mark error
      if ((article != null) &&
          ((article.length() > 0) || (local && (link.getText() != null))) &&
          (prefix != null) && (prefix.length() > 0) &&
          (fromWiki != wiki) &&
          (!onlyLanguage || (language != null)) &&
          (!onlyLocal || local)) {
        if (errors == null) {
          return true;
        }
        result = true;
        int beginIndex = link.getBeginIndex();
        int endIndex = link.getEndIndex();
        if ((beginIndex > 0) && (contents.charAt(beginIndex - 1) == '[') &&
            (endIndex < contents.length()) && (contents.charAt(endIndex) == ']')) {
          beginIndex--;
          endIndex++;
        }

        // Check language link
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis, beginIndex, endIndex);
        if ((fromWiki != null) && (article.length() >0)) {
          errorResult.addPossibleAction(
              GT._("Check language links"),
              new CheckLanguageLinkActionProvider(
                  fromWiki, analysis.getWikipedia(),
                  article, link.getText()));
        }

        // Use templates
        if ((templatesList != null) &&
            (templatesList.size() > 0) &&
            (article.length() > 0) &&
            (language != null)) {
          for (String template : templatesList) {
            String[] templateArgs = template.split("\\|");
            if (templateArgs.length >= 5) {
              String textPrefix =
                "{{" + templateArgs[0] + "|" + templateArgs[1] + "=";
              String textSuffix =
                "|" + templateArgs[2] + "=" + prefix +
                "|" + templateArgs[3] + "=" + article +
                "|" + templateArgs[4] + "=" + ((link.getText() != null) ? link.getText() : article) +
                "}}";
              String question = GT._("What is the title of the page on this wiki ?");
              AddTextActionProvider action = null;
              if ((link.getText() != null) && (!link.getText().equals(article))) {
                String[] possibleValues = { null, article, link.getText() };
                action = new AddTextActionProvider(
                    textPrefix, textSuffix, null, question,
                    possibleValues, false, null, checker);
              } else {
                action = new AddTextActionProvider(
                    textPrefix, textSuffix, null, question,
                    article, checker);
              }
              errorResult.addPossibleAction(
                  GT._("Replace using template {0}", "{{" + templateArgs[0] + "}}"),
                  action);
            }
          }
        }

        // Create internal link
        if (!link.hasSquare() || link.hasSecondSquare()) {
          boolean first = (errorResult.getPossibleActions() == null) || (errorResult.getPossibleActions().isEmpty());
          errorResult.addReplacement(
              "[[:" + prefix + ":" + article + "|" + (link.getText() != null ? link.getText() : article) + "]]",
              first);
        }
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
    parameters.put("only_language", GT._("To report only links to other languages"));
    parameters.put("only_local", GT._("To report only links to local wikis"));
    return parameters;
  }
}
