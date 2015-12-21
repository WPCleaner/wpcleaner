/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.check.AddTextActionProvider;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CheckLanguageLinkActionProvider;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.constants.CWConfigurationError;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.data.Interwiki;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.PageElementTemplate;
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
   * Possible separation characters at the end of the internal link. 
   */
  private final static String SEPARATION_CHARACTERS = ",.";

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
    String contents = analysis.getContents();
    for (PageElementExternalLink link : links) {

      // Check if this is a external link to an other wiki
      boolean fullLink = true;
      String linkDest = link.getLink();
      Interwiki interwiki = getInterwiki(wiki, linkDest);
      if (interwiki == null) {
        try {
          URL url = new URL(linkDest);
          String query = url.getQuery();
          if ((query != null) && (query.length() > 0)) {
            String[] elements = query.split("\\&");
            for (String element : elements) {
              String[] parts = element.split("\\=");
              if ((parts != null) && (parts.length > 1)) {
                Interwiki tmp = getInterwiki(wiki, parts[1]);
                if (tmp != null) {
                  interwiki = tmp;
                  linkDest = parts[1];
                  fullLink = false;
                }
              }
            }
          }
        } catch (MalformedURLException e) {
          // Nothing to be done
        }
      }
      String article = null;
      if (interwiki != null) {
        article = interwiki.isArticleUrl(linkDest);
      }

      if ((interwiki != null) && (article != null)) {
        // Decide if error should be reported
        String prefix = interwiki.getPrefix();
        String language = interwiki.getLanguage();
        boolean local = interwiki.getLocal();
        EnumWikipedia fromWiki = null;
        if (prefix != null) {
          fromWiki = EnumWikipedia.getWikipedia(prefix);
          if (!prefix.equals(fromWiki.getSettings().getCode())) {
            fromWiki = null;
          }
        }
        boolean isError = true;
        if (isError && (article.length() == 0) && (!local || link.getText() == null)) {
          isError = false;
        }
        if (fromWiki == wiki) {
          isError = false;
        }
        if ((prefix == null) || (prefix.length() == 0)) {
          isError = false;
        }
        if (onlyLanguage && (language == null)) {
          isError = false;
        }
        if (onlyLocal && !local) {
          isError = false;
        }
  
        // Mark error
        if (isError) {
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
  
          // Check language link
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, beginIndex, endIndex,
              fullLink ? ErrorLevel.ERROR : ErrorLevel.WARNING);
          if ((fromWiki != null) && (article.length() >0)) {
            errorResult.addPossibleAction(
                GT._("Check language links"),
                new CheckLanguageLinkActionProvider(
                    fromWiki, analysis.getWikipedia(),
                    article, text));
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
                  "|" + templateArgs[4] + "=" + ((text != null) ? text : article) +
                  "}}";
                String question = GT._("What is the title of the page on this wiki ?");
                AddTextActionProvider action = null;
                if ((text != null) && (!text.equals(article))) {
                  String[] possibleValues = { null, article, text };
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
            int lastSure = article.length();
            while ((lastSure > 0) &&
                   (SEPARATION_CHARACTERS.indexOf(article.charAt(lastSure - 1)) >= 0)) {
              lastSure--;
            }
            if ((text == null) && (lastSure < article.length())) {
              while (lastSure <= article.length()) {
                String actualArticle = article.substring(0, lastSure);
                errorResult.addReplacement(
                    "[[:" + prefix + ":" + actualArticle + "|" + actualArticle + "]]" + article.substring(lastSure));
                lastSure++;
              }
            } else {
              boolean first = (errorResult.getPossibleActions() == null) || (errorResult.getPossibleActions().isEmpty());
              errorResult.addReplacement(
                  "[[:" + prefix + ":" + article + "|" + (text != null ? text : article) + "]]",
                  first && fullLink);
            }
          }
          errors.add(errorResult);
        }
      }
    }

    return result;
  }

  /**
   * @param wiki Current wiki.
   * @param link External link.
   * @return Interwiki matching the external link if it exists.
   */
  private Interwiki getInterwiki(EnumWikipedia wiki, String link) {
    Interwiki result = null;
    List<Interwiki> interwikis = wiki.getWikiConfiguration().getInterwikis();
    if (interwikis != null) {
      for (Interwiki interwiki : interwikis) {
        String tmp = interwiki.isArticleUrl(link);
        if (tmp != null) {
          if ((result == null) || (interwiki.getLanguage() != null)) {
            result = interwiki;
          }
        }
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
    parameters.put("only_language", GT._("To report only links to other languages"));
    parameters.put("only_local", GT._("To report only links to local wikis"));
    return parameters;
  }
}
