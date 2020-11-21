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

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.AddTextActionProvider;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.BasicActionProvider;
import org.wikipediacleaner.api.check.CheckLanguageLinkActionProvider;
import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Interwiki;
import org.wikipediacleaner.api.data.PageElementInterwikiLink;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.ilink.InternalLinkBuilder;
import org.wikipediacleaner.api.data.contents.template.TemplateBuilder;
import org.wikipediacleaner.gui.swing.action.ActionExternalViewer;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.StringChecker;
import org.wikipediacleaner.utils.StringCheckerUnauthorizedCharacters;


/**
 * Algorithm for analyzing error 68 of check wikipedia project.
 * Error 68: Link to other language
 */
public class CheckErrorAlgorithm068 extends CheckErrorAlgorithmBase {

  /**
   * String checker for text inputed by user.
   */
  private final StringChecker checker;

  /**
   * Possible global fixes.
   */
  private final static String[] globalFixes = new String[] {
    GT._T("Check all links to other language"),
  };

  public CheckErrorAlgorithm068() {
    super("Link to other language");
    checker = new StringCheckerUnauthorizedCharacters("[]\"");
  }

  /**
   * @param link Interwiki link.
   * @param wiki Current wiki.
   * @return True if the interwiki link is a link to an other language.
   */
  private Interwiki isLanguageLink(PageElementInterwikiLink link, EnumWikipedia wiki) {
    if ((link != null) &&
        (link.getInterwiki() != null)) {
      if (link.getInterwiki().getLanguage() != null) {
        if (!link.getInterwikiText().equals(wiki.getSettings().getCode())) {
          return link.getInterwiki();
        }
      } else {
        List<Interwiki> iws = wiki.getWikiConfiguration().getInterwikis();
        for (Interwiki iw : iws) {
          if ((iw.getLanguage() != null) &&
              (iw.getURL().equals(link.getInterwiki().getURL()))) {
            return iw;
          }
        }
      }
    }
    return null;
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
      Collection<CheckErrorResult> errors,
      boolean onlyAutomatic) {
    if ((analysis == null) || (analysis.getPage() == null)) {
      return false;
    }
    if (!analysis.getPage().isArticle()) {
      return false;
    }

    // Analyzing the text from the beginning
    boolean result = false;
    EnumWikipedia toWiki = analysis.getWikipedia();
    for (PageElementInterwikiLink link : analysis.getInterwikiLinks()) {
      Interwiki iw = isLanguageLink(link, toWiki);
      if (iw != null) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis, link.getBeginIndex(), link.getEndIndex());
        String lgCode = iw.getPrefix();
        EnumWikipedia fromWiki = EnumWikipedia.getWikipedia(lgCode);
        if ((fromWiki != null) && (fromWiki.getSettings().getCode().equals(lgCode))) {
          String pageTitle = link.getLink();
          errorResult.addPossibleAction(
              GT._T("Check language links"),
              new CheckLanguageLinkActionProvider(
                  fromWiki, toWiki,
                  pageTitle, link.getText()));
          for (String[] templateArgs : templatesArgs) {
            StringBuilder prefix = new StringBuilder();
            StringBuilder suffix = new StringBuilder();
            buildReplacementTemplate(templateArgs, link, prefix, suffix);
            String question = GT._T("What is the title of the page on this wiki ?");
            AddTextActionProvider action = null;
            if ((link.getText() != null) && (!link.getText().equals(pageTitle))) {
              String[] possibleValues = { null, pageTitle, link.getText() };
              action = new AddTextActionProvider(
                  prefix.toString(), suffix.toString(), null, question,
                  possibleValues, false, null, checker);
            } else {
              action = new AddTextActionProvider(
                  prefix.toString(), suffix.toString(), null, question,
                  pageTitle, checker);
            }
            errorResult.addPossibleAction(
                GT._T("Replace using template {0}", TemplateBuilder.from(templateArgs[0]).toString()),
                action);
          }
          errorResult.addPossibleAction(
              GT._T("External Viewer"),
              new BasicActionProvider(
                  new ActionExternalViewer(fromWiki, pageTitle)));
        }
        errors.add(errorResult);
      }
    }

    return result;
  }

  /**
   * Build a replacement text with a template.
   * 
   * @param templateArgs Configuration for the template.
   * @param link Link to the other wiki.
   * @param prefix Prefix for the replacement text (output).
   * @param suffix Suffix for the replacement text (output).
   */
  private void buildReplacementTemplate(
      String[] templateArgs, PageElementInterwikiLink link,
      StringBuilder prefix, StringBuilder suffix) {

    // Parameters
    String templateName = templateArgs[0];
    String paramLocalTitle = templateArgs[1];
    String paramLang = templateArgs[2];
    String defaultLang = null;
    if (paramLang.indexOf('=') > 0) {
      int equalIndex = paramLang.indexOf('=');
      defaultLang = paramLang.substring(equalIndex + 1);
      paramLang = paramLang.substring(0, equalIndex);
    }
    String paramDistantTitle = templateArgs[3];
    String paramText = templateArgs[4];

    // Compute order of parameters
    List<String> order = new ArrayList<>();
    if (templateArgs.length >= 6) {
      String[] tmpOrder = templateArgs[5].split(",");
      for (String tmp : tmpOrder) {
        order.add(tmp);
      }
    }
    if (!order.contains(paramLocalTitle)) {
      order.add(paramLocalTitle);
    }
    if (!order.contains(paramLang)) {
      order.add(paramLang);
    }
    if (!order.contains(paramDistantTitle)) {
      order.add(paramDistantTitle);
    }
    if (!order.contains(paramText)) {
      order.add(paramText);
    }

    // Build text
    prefix.append("{{");
    prefix.append(templateName);
    boolean localTitle = false;
    String lgCode = link.getInterwiki().getPrefix();
    for (String param : order) {
      if (param.equals(paramLocalTitle)) {
        prefix.append("|");
        prefix.append(paramLocalTitle);
        prefix.append("=");
        localTitle = true;
      } else {
        String value = "";
        boolean append = true;
        if (param.equals(paramLang)) {
          if ((defaultLang != null) && defaultLang.equals(lgCode)) {
            append = false;
          } else {
            value = lgCode;
          } 
        } else if (param.equals(paramDistantTitle)) {
          value = link.getLink();
        } else if (param.equals(paramText)) {
          value = (link.getText() != null) ? link.getText() : link.getLink();
        }
        if (append) {
          StringBuilder buffer = localTitle ? suffix : prefix;
          buffer.append("|");
          buffer.append(param);
          buffer.append("=");
          if (value != null) {
            buffer.append(value);
          }
        }
      }
    }
    suffix.append("}}");
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

    // Initialize
    API api = APIFactory.getAPI();
    StringBuilder tmpContents = new StringBuilder();
    int currentIndex = 0;

    // Manage templates that can be used to replace a link to an other language
    String[] templateArgs = templatesArgs.isEmpty() ? null : templatesArgs.get(0);

    // Check all internal links
    Object highlight = null;
    String contents = analysis.getContents();
    try {
      EnumWikipedia toWiki = analysis.getWikipedia();
      for (PageElementInterwikiLink link : analysis.getInterwikiLinks()) {
        Interwiki iw = isLanguageLink(link, toWiki);
        if (iw != null) {
          String lgCode = iw.getPrefix();
          EnumWikipedia fromWiki = EnumWikipedia.getWikipedia(lgCode);
          if ((fromWiki != null) && (fromWiki.getSettings().getCode().equals(lgCode))) {
            String pageTitle = link.getLink();
            int beginIndex = link.getBeginIndex();
            int endIndex = link.getEndIndex();
            String replacement = null;

            // Display selection
            highlight = addHighlight(textPane, beginIndex, endIndex);
            textPane.select(beginIndex, endIndex);

            // Check for language link
            String toTitle = api.getLanguageLink(fromWiki, toWiki, pageTitle);
            if (toTitle != null) {

              // List possible replacements
              List<String> possibleValues = new ArrayList<String>();
              String possible = null;
              possible = InternalLinkBuilder.from(toTitle).withText(link.getText()).toString();
              if (!possibleValues.contains(possible)) {
                possibleValues.add(possible);
              }
              possible = InternalLinkBuilder.from(toTitle).withText(link.getFullLink()).toString();
              if (!possibleValues.contains(possible)) {
                possibleValues.add(possible);
              }
              possible = InternalLinkBuilder.from(toTitle).toString();
              if (!possibleValues.contains(possible)) {
                possibleValues.add(possible);
              }
              possibleValues.add(GT._T("Do not replace"));
              possibleValues.add(GT._T("Cancel"));

              // Ask user what replacement to use
              String message = GT._T(
                  "The page \"{0}\" in \"{1}\" has a language link to \"{2}\": {3}.\n" +
                  "With what text do you want to replace the link ?",
                  new Object[] { pageTitle, fromWiki, toWiki, toTitle } );
              int answer = Utilities.displayQuestion(
                  textPane.getParent(), message,
                  possibleValues.toArray());
              if ((answer < 0) || (answer >= possibleValues.size() - 1)) {
                break;
              } else if (answer < possibleValues.size() - 2) {
                replacement = possibleValues.get(answer);
              }
            } else if (templateArgs != null) {
              String message =
                  GT._T("The page \"{0}\" in \"{1}\" doesn''t have a language link to \"{2}\".",
                       new Object[] { pageTitle, fromWiki, toWiki }) +"\n" +
                  GT._T("You can replace the link using template {0}.",
                       TemplateBuilder.from(templateArgs[0]).toString()) + "\n" +
                  GT._T("What is the title of the page on this wiki ?");
              if ((link.getText() != null) && (!link.getText().equals(pageTitle))) {
                toTitle = Utilities.askForValue(
                    textPane.getParent(), message, link.getText(), checker);
              } else {
                toTitle = Utilities.askForValue(
                    textPane.getParent(), message, pageTitle, checker);
              }
              if (toTitle != null) {
                StringBuilder prefix = new StringBuilder();
                StringBuilder suffix = new StringBuilder();
                buildReplacementTemplate(templateArgs, link, prefix, suffix);
                replacement = prefix.toString() + toTitle + suffix.toString();
              }
            }

            // Do the replacement
            if (replacement != null) {
              if (currentIndex < link.getBeginIndex()) {
                tmpContents.append(contents.substring(currentIndex, link.getBeginIndex()));
              }
              tmpContents.append(replacement);
              currentIndex = link.getEndIndex();
            }
            removeHighlight(textPane, highlight);
            highlight = null;
          }
        }
      }
    } catch (APIException e) {
      //
    }
    removeHighlight(textPane, highlight);
    highlight = null;

    // Return result
    if (currentIndex == 0) {
      return contents;
    }
    if (currentIndex < contents.length()) {
      tmpContents.append(contents.substring(currentIndex));
    }
    return tmpContents.toString();
  }

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** Templates used instead of the link */
  public static final String PARAMETER_TEMPLATE = "template";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    String tmp = getSpecificProperty(PARAMETER_TEMPLATE, true, false, false);
    templatesArgs.clear();
    if (tmp != null) {
      List<String[]> tmpList = WPCConfiguration.convertPropertyToStringArrayList(tmp);
      if (tmpList != null) {
        for (String[] tmpItem : tmpList) {
          if (tmpItem.length >= 5) {
            templatesArgs.add(tmpItem);
          }
        }
      }
    }
  }

  /** Templates used instead of the link */
  private final List<String[]> templatesArgs = new ArrayList<>();

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_TEMPLATE,
        GT._T("A template that can be used instead of the link to an other language."),
        new AlgorithmParameterElement[] {
            new AlgorithmParameterElement(
                "template name",
                GT._T("A template that can be used instead of the link to an other language.")),
            new AlgorithmParameterElement(
                "local page name",
                GT._T("Parameter name for the local page name")),
            new AlgorithmParameterElement(
                "code of other language",
                GT._T("Parameter name for the code of the other language")),
            new AlgorithmParameterElement(
                "page name in other language",
                GT._T("Parameter name for the page name in the other language")),
            new AlgorithmParameterElement(
                "displayed text",
                GT._T("Parameter name for the displayed text"))
        },
        false));
  }
}
