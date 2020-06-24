/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.AddTextActionProvider;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.BasicActionProvider;
import org.wikipediacleaner.api.check.CheckLanguageLinkActionProvider;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.data.Interwiki;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.gui.swing.action.ActionExternalViewer;
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
    GT._T("Convert them to internal links"),
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
    if (!analysis.getPage().isArticle()) {
      return false;
    }

    // Analyze each external link
    boolean result = false;
    List<PageElementExternalLink> links = analysis.getExternalLinks();
    if (links == null) {
      return result;
    }
    String contents = analysis.getContents();
    EnumWikipedia wiki = analysis.getWikipedia();
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
      String articleName = null;
      if (interwiki != null) {
        article = interwiki.isArticleUrl(linkDest);
        if (article != null) {
          int questionMark = article.indexOf('?');
          if (questionMark < 0) {
            articleName = article;
          } else {
            articleName = article.substring(0, questionMark);
            fullLink = false;
          }
        }
      }

      if ((interwiki != null) && (article != null) && (articleName != null)) {
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
  
          // Compute informations
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
          if ((fromWiki != null) && (articleName.length() >0)) {
            errorResult.addPossibleAction(
                GT._T("Check language links"),
                new CheckLanguageLinkActionProvider(
                    fromWiki, analysis.getWikipedia(),
                    articleName, text));
          }
  
          // Use templates
          if ((templatesList != null) &&
              (templatesList.size() > 0) &&
              (articleName.length() > 0) &&
              (language != null)) {
            for (String template : templatesList) {
              String[] templateArgs = template.split("\\|");
              if (templateArgs.length >= 5) {
                String templateName = templateArgs[0];
                String paramLocalTitle = templateArgs[1];
                String paramLang = templateArgs[2];
                String paramLangDefaultValue = null;
                int equalIndex = paramLang.indexOf('=');
                if (equalIndex > 0) {
                  paramLangDefaultValue = paramLang.substring(equalIndex + 1);
                  paramLang = paramLang.substring(0, equalIndex);
                }
                String paramOriginalTitle = templateArgs[3];
                String paramText = templateArgs[4];
                String textPrefix =
                  "{{" + templateName + "|" + paramLocalTitle + "=";
                StringBuilder textSuffix = new StringBuilder();
                if ((prefix != null) && !prefix.equals(paramLangDefaultValue)) {
                  textSuffix.append('|');
                  textSuffix.append(paramLang);
                  textSuffix.append('=');
                  textSuffix.append(prefix);
                }
                textSuffix.append('|');
                textSuffix.append(paramOriginalTitle);
                textSuffix.append('=');
                textSuffix.append(articleName);
                textSuffix.append('|');
                textSuffix.append(paramText);
                textSuffix.append('=');
                textSuffix.append((text != null) ? text : article);
                textSuffix.append("}}");
                String question = GT._T("What is the title of the page on this wiki ?");
                AddTextActionProvider action = null;
                if ((text != null) && (!text.equals(article))) {
                  String[] possibleValues = { null, article, text };
                  action = new AddTextActionProvider(
                      textPrefix, textSuffix.toString(), null, question,
                      possibleValues, false, null, checker);
                } else {
                  action = new AddTextActionProvider(
                      textPrefix, textSuffix.toString(), null, question,
                      articleName, checker);
                }
                errorResult.addPossibleAction(
                    GT._T("Replace using template {0}", "{{" + templateArgs[0] + "}}"),
                    action);
              }
            }
          }
  
          // Create internal link
          if (articleName.length() > 0) {
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
                    first && fullLink && link.hasSquare() && link.hasSecondSquare() && (text != null));
              }
            }
          }

          // External viewer
          errorResult.addPossibleAction(
              GT._T("External Viewer"),
              new BasicActionProvider(
                  new ActionExternalViewer(link.getLink())));

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
    return fixUsingAutomaticBotReplacement(analysis);
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

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** Parameter to report only links to other languages */
  private static final String PARAMETER_ONLY_LANGUAGE = "only_language";

  /** Parameter to report only links to local wikis */
  private static final String PARAMETER_ONLY_LOCAL = "only_local";

  /** Parameter for templates using external links */
  private static final String PARAMETER_LINK_TEMPLATES = "link_templates";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    String tmp = getSpecificProperty(PARAMETER_ONLY_LANGUAGE, true, true, false);
    onlyLanguage = (tmp != null) ? Boolean.valueOf(tmp) : true;

    tmp = getSpecificProperty(PARAMETER_ONLY_LOCAL, true, false, false);
    onlyLocal = (tmp != null) ? Boolean.valueOf(tmp) : true;

    tmp = getSpecificProperty(PARAMETER_LINK_TEMPLATES, true,  true,  false);
    linkTemplates.clear();
    if (tmp != null) {
      List<String> tmpList = WPCConfiguration.convertPropertyToStringList(tmp);
      if (tmpList != null) {
        linkTemplates.addAll(tmpList);
      }
    }

    tmp = getSpecificProperty(68, CheckErrorAlgorithm068.PARAMETER_TEMPLATE, true, false, false);
    templatesList.clear();
    if (tmp != null) {
      List<String> tmpList = WPCConfiguration.convertPropertyToStringList(tmp);
      if (tmpList != null) {
        templatesList.addAll(tmpList);
      }
    }
  }

  /** True to report only links to other languages */
  private boolean onlyLanguage = true;

  /** True to report only links to local wikis */
  private boolean onlyLocal = true;

  /** Templates using external links */
  private final List<String> linkTemplates = new ArrayList<>();

  /** Templates */
  private final List<String> templatesList = new ArrayList<>();

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_LINK_TEMPLATES,
        GT._T("Templates using external links"),
        new AlgorithmParameterElement[] {
            new AlgorithmParameterElement(
                "template name",
                GT._T("Template")),
            new AlgorithmParameterElement(
                "link param",
                GT._T("Parameter to be used for the link"),
                true),
            new AlgorithmParameterElement(
                "text param",
                GT._T("Parameter to be used for the text"),
                true)
        },
        true));
    addParameter(new AlgorithmParameter(
        PARAMETER_ONLY_LANGUAGE,
        GT._T("To report only links to other languages"),
        new AlgorithmParameterElement(
            "true/false",
            GT._T("To report only links to other languages"))));
    addParameter(new AlgorithmParameter(
        PARAMETER_ONLY_LOCAL,
        GT._T("To report only links to local wikis"),
        new AlgorithmParameterElement(
            "true/false",
            GT._T("To report only links to local wikis"))));
  }
}
