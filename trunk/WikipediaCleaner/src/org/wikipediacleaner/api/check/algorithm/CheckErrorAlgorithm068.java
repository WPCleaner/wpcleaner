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
import org.wikipediacleaner.api.check.BasicActionProvider;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CheckLanguageLinkActionProvider;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementInterwikiLink;
import org.wikipediacleaner.gui.swing.action.ActionExternalViewer;
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

  public CheckErrorAlgorithm068() {
    super("Link to other language");
    checker = new StringCheckerUnauthorizedCharacters("[]\"");
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
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param onlyAutomatic True if analysis could be restricted to errors automatically fixed.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      boolean onlyAutomatic) {
    if (analysis == null) {
      return false;
    }

    // Retrieve possible templates to replace the link to other language
    List<String> templatesList = getTemplatesList();

    // Analyzing the text from the beginning
    boolean result = false;
    EnumWikipedia wiki = analysis.getWikipedia();
    for (PageElementInterwikiLink link : analysis.getInterwikiLinks()) {
      if ((link != null) &&
          (link.getInterwiki() != null) &&
          (link.getInterwiki().getLanguage() != null) &&
          (!link.getInterwikiText().equals(wiki.getSettings().getCode()))) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis.getPage(), link.getBeginIndex(), link.getEndIndex());
        String lgCode = link.getInterwiki().getPrefix();
        EnumWikipedia fromWiki = EnumWikipedia.getWikipedia(lgCode);
        if ((fromWiki != null) && (fromWiki.getSettings().getCode().equals(lgCode))) {
          String pageTitle = link.getLink();
          errorResult.addPossibleAction(
              GT._("Check language links"),
              new CheckLanguageLinkActionProvider(
                  fromWiki, wiki,
                  pageTitle, link.getText()));
          if ((templatesList != null) && (templatesList.size() > 0)) {
            for (String template : templatesList) {
              String[] templateArgs = template.split("\\|");
              if (templateArgs.length >= 5) {
                String prefix =
                  "{{" + templateArgs[0] + "|" + templateArgs[1] + "=";
                String suffix =
                  "|" + templateArgs[2] + "=" + fromWiki.getSettings().getCode() +
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
                  new ActionExternalViewer(fromWiki, pageTitle)));
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
}
