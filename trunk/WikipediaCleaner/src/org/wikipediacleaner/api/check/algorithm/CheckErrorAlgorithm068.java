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
import java.util.Map;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.check.AddTextActionProvider;
import org.wikipediacleaner.api.check.BasicActionProvider;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CheckLanguageLinkActionProvider;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.data.Language;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementInternalLink;
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
    GT._("Check all links to other language"),
  };

  public CheckErrorAlgorithm068() {
    super("Link to other language");
    checker = new StringCheckerUnauthorizedCharacters("[]\"");
  }

  /**
   * Local class for holding analysis result.
   */
  private static class ErrorAnalysis {
    final Language language;
    final EnumWikipedia fromWiki;
    final String title;

    ErrorAnalysis(Language language, String title) {
      this.language = language;
      this.fromWiki = EnumWikipedia.getWikipedia(language.getCode());
      this.title = title;
    }
  }

  /**
   * Check if a link is to an other language.
   * 
   * @param link Link.
   * @param pageAnalysis Page analysis.
   * @return Language to which the link is pointing to.
   */
  private ErrorAnalysis isLinkToOtherLanguage(
      PageElementInternalLink link, PageAnalysis pageAnalysis) {
    // Check that link starts with :
    String linkUrl = link.getLink();
    if ((linkUrl == null) || (!linkUrl.startsWith(":"))) {
      return null;
    }

    // Check that there's a second :
    int currentPos = linkUrl.indexOf(":", 1);
    if (currentPos < 0) {
      return null;
    }

    // Check that it's not a link to the current wiki
    String namespace = linkUrl.substring(1, currentPos);
    if (namespace.equals(pageAnalysis.getSettings().getCode())) {
      return null;
    }

    // Check that it's a link to an other wiki
    for (Language lg : pageAnalysis.getWikiConfiguration().getLanguages()) {
      if (namespace.equals(lg.getCode())) {
        return new ErrorAnalysis(lg, linkUrl.substring(currentPos + 1));
      }
    }

    return null;
  }

  /**
   * @return Possible templates to replace the link to an other language.
   */
  private List<String> getTemplatesList() {
    String templatesParam = getSpecificProperty(
        "template", true, false, false);
    List<String> templatesList = null;
    if (templatesParam != null) {
      templatesList = WPCConfiguration.convertPropertyToStringList(templatesParam);
    }
    return templatesList;
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param pageAnalysis Page analysis.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(
      PageAnalysis pageAnalysis,
      Collection<CheckErrorResult> errors) {
    if (pageAnalysis == null) {
      return false;
    }

    // Retrieve possible templates to replace the link to other language
    List<String> templatesList = getTemplatesList();

    // Analyzing the text from the beginning
    boolean result = false;
    for (PageElementInternalLink link : pageAnalysis.getInternalLinks()) {

      // Check if it is a link to an other language
      ErrorAnalysis analysis = isLinkToOtherLanguage(link, pageAnalysis);
      if (analysis != null) {
        if (errors == null) {
          return true;
        }
        result = true;

        String lgCode = analysis.language.getCode();
        String pageTitle = analysis.title;
        EnumWikipedia fromWikipedia = analysis.fromWiki;

        CheckErrorResult errorResult = createCheckErrorResult(
            pageAnalysis.getPage(), link.getBeginIndex(), link.getEndIndex());
        if (fromWikipedia != null) {
          errorResult.addPossibleAction(
              GT._("Check language links"),
              new CheckLanguageLinkActionProvider(
                  fromWikipedia, pageAnalysis.getWikipedia(),
                  pageTitle, link.getText()));
          if ((templatesList != null) && (templatesList.size() > 0)) {
            for (String template : templatesList) {
              String[] templateArgs = template.split("\\|");
              if (templateArgs.length >= 5) {
                String prefix =
                  "{{" + templateArgs[0] + "|" + templateArgs[1] + "=";
                String suffix =
                  "|" + templateArgs[2] + "=" + lgCode +
                  "|" + templateArgs[3] + "=" + pageTitle +
                  "|" + templateArgs[4] + "=" + ((link.getText() != null) ? link.getText() : pageTitle) +
                  "}}";
                String question = GT._("What is the title of the page on this wiki ?");
                AddTextActionProvider action = null;
                if ((link.getText() != null) && (!link.getText().equals(pageTitle))) {
                  String[] possibleValues = { null, pageTitle, link.getText() };
                  action = new AddTextActionProvider(
                      prefix, suffix, null, question,
                      possibleValues, false, null, checker);
                } else {
                  action = new AddTextActionProvider(
                      prefix, suffix, null, question,
                      pageTitle, checker);
                }
                errorResult.addPossibleAction(
                    GT._("Replace using template {0}", "{{" + templateArgs[0] + "}}"),
                    action);
              }
            }
          }
          errorResult.addPossibleAction(
              GT._("External Viewer"),
              new BasicActionProvider(
                  new ActionExternalViewer(fromWikipedia, pageTitle)));
        }
        errors.add(errorResult);
      }
    }
    return result;
  }

  /**
   * Return the parameters used to configure the algorithm.
   * 
   * @return Map of parameters (Name -> description).
   */
  @Override
  public Map<String, String> getParameters() {
    Map<String, String> parameters = super.getParameters();
    parameters.put("template", GT._(
        "A template that can be used instead of the link to an other language. " +
        "It must be specified as: " +
          "<template name>|" +
          "<param name for local page name>|" +
          "<param name for code of other language>|" +
          "<param name for page name in other language>|" +
          "<param name for displayed text>").replaceAll("\\<", "&lt;").replaceAll("\\>", "&gt;"));
    return parameters;
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
    List<String> templatesList = getTemplatesList();
    String[] templateArgs = null;
    if ((templatesList != null) && (templatesList.size() > 0)) {
      String[] tmp = templatesList.get(0).split("\\|");
      if (tmp.length >= 5) {
        templateArgs = tmp;
      }
    }

    // Check all internal links
    Object highlight = null;
    String contents = analysis.getContents();
    try {
      for (PageElementInternalLink link : analysis.getInternalLinks()) {
        ErrorAnalysis errorAnalysis = isLinkToOtherLanguage(link, analysis);
        if ((errorAnalysis != null) && (errorAnalysis.fromWiki != null)) {
          EnumWikipedia fromWiki = errorAnalysis.fromWiki;
          EnumWikipedia toWiki = analysis.getWikipedia();
          String pageTitle = errorAnalysis.title;
          String lgCode = errorAnalysis.language.getCode();
          String replacement = null;

          // Display selection
          highlight = addHighlight(
              textPane, link.getBeginIndex(), link.getEndIndex());
          textPane.select(link.getBeginIndex(), link.getEndIndex());

          // Check for language link
          String toTitle = api.getLanguageLink(fromWiki, toWiki, pageTitle);
          if (toTitle != null) {

            // List possible replacements
            List<String> possibleValues = new ArrayList<String>();
            String possible = PageElementInternalLink.createInternalLink(
                toTitle, link.getText());
            if (!possibleValues.contains(possible)) {
              possibleValues.add(possible);
            }
            possible = PageElementInternalLink.createInternalLink(
                toTitle, link.getDisplayedText());
            if (!possibleValues.contains(possible)) {
              possibleValues.add(possible);
            }
            possible = PageElementInternalLink.createInternalLink(
                toTitle, null);
            if (!possibleValues.contains(possible)) {
              possibleValues.add(possible);
            }
            possibleValues.add(GT._("Do not replace"));
            possibleValues.add(GT._("Cancel"));

            // Ask user what replacement to use
            String message = GT._(
                "The page \"{0}\" in \"{1}\" has a language link to \"{2}\": {3}.\n" +
                "By what text do you want to replace the link ?",
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
                GT._("The page \"{0}\" in \"{1}\" doesn''t have a language link to \"{2}\".",
                     new Object[] { pageTitle, fromWiki, toWiki }) +"\n" +
                GT._("You can replace the link using template {0}.",
                     "{{" + templateArgs[0] + "}}") + "\n" +
                GT._("What is the title of the page on this wiki ?");
            if ((link.getText() != null) && (!link.getText().equals(pageTitle))) {
              toTitle = Utilities.askForValue(
                  textPane.getParent(), message, link.getText(), checker);
            } else {
              toTitle = Utilities.askForValue(
                  textPane.getParent(), message, pageTitle, checker);
            }
            if (toTitle != null) {
              replacement =
                  "{{" + templateArgs[0] +
                  "|" + templateArgs[1] + "=" + toTitle +
                  "|" + templateArgs[2] + "=" + lgCode +
                  "|" + templateArgs[3] + "=" + pageTitle +
                  "|" + templateArgs[4] + "=" + ((link.getText() != null) ? link.getText() : pageTitle) +
                  "}}";
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
}
