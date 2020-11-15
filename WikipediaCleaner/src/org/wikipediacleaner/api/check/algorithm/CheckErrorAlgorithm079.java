/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;

import org.wikipediacleaner.api.check.AddTextActionProvider;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.SimpleAction;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.tag.ContentsFullTagBuilder;
import org.wikipediacleaner.api.data.contents.tag.ContentsTagBuilder;
import org.wikipediacleaner.gui.swing.action.ActionExternalViewer;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.StringChecker;
import org.wikipediacleaner.utils.StringCheckerUnauthorizedCharacters;
import org.wikipediacleaner.utils.TextProviderUrlTitle;


/**
 * Algorithm for analyzing error 79 of check wikipedia project.
 * Error 79: External link without description.
 */
public class CheckErrorAlgorithm079 extends CheckErrorAlgorithmBase {

  /**
   * StringChecker for the description.
   */
  private final StringChecker descriptionChecker;

  public CheckErrorAlgorithm079() {
    super("External link without description");
    descriptionChecker = new StringCheckerUnauthorizedCharacters("[]");
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

    boolean result = false;
    Collection<PageElementExternalLink> links = analysis.getExternalLinks();
    String contents = analysis.getContents();
    for (PageElementExternalLink link : links) {
      String text = link.getText();
      if ((text == null) || (text.trim().length() == 0)) {
        boolean hasError = false;
        PageElementTag refTag = analysis.getSurroundingTag(
            PageElementTag.TAG_WIKI_REF, link.getBeginIndex());
        if (link.hasSquare()) {
          hasError = true;
        } else {
          PageElementTemplate template = analysis.isInTemplate(link.getBeginIndex());
          if (template == null) {
            if (refTag != null) {
              hasError = true;
            }
          }
        }
        if (hasError) {
          if (errors == null) {
            return true;
          }
          result = true;
          String url = link.getLink();
          int endIndex = link.getEndIndex();
          String suffix =  "";
          if ((refTag != null) &&
              (refTag.getMatchingTag() != null) &&
              (refTag.getMatchingTag().getBeginIndex() > endIndex)) {
            while ((endIndex < refTag.getMatchingTag().getBeginIndex()) &&
                   (contents.charAt(endIndex) != '[') &&
                   (contents.charAt(endIndex) != ']')) {
              endIndex++;
            }
            if (endIndex > link.getEndIndex()) {
              String tmp = contents.substring(link.getEndIndex(), endIndex);
              if (tmp.trim().length() > 0) {
                suffix = tmp;
              }
            }
          }
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, link.getBeginIndex(), endIndex);
          errorResult.addPossibleAction(
              GT._T("Add a description..."),
              new AddTextActionProvider(
                  "[" + url + " ", "]" + suffix,
                  new TextProviderUrlTitle(url),
                  GT._T("What description would you like to use for the external link ?"),
                  descriptionChecker));
          if (refTag == null) {
            errorResult.addReplacement(
                ContentsFullTagBuilder.from(PageElementTag.TAG_WIKI_REF, url).toString(),
                GT._T("Convert into <ref> tag"));
            errorResult.addPossibleAction(
                GT._T("Add a description and convert into <ref> tag"),
                new AddTextActionProvider(
                    ContentsTagBuilder.REF_OPEN + "[" + url + " ", "]" + ContentsTagBuilder.REF_CLOSE,
                    new TextProviderUrlTitle(url),
                    GT._T("What description would you like to use for the external link ?"),
                    descriptionChecker));
          } else {
            if (suffix.length() > 0) {
              if (link.hasSquare()) {
                errorResult.addReplacement(PageElementExternalLink.createExternalLink(url, suffix.trim()));
              } else {
                errorResult.addReplacement("[" + contents.substring(link.getBeginIndex(), endIndex) + "]");
              }
            }
            if (link.hasSquare()) {
              errorResult.addReplacement(url + suffix);
            }
          }
          errorResult.addPossibleAction(
              new SimpleAction(GT._T("External viewer"),
                  new ActionExternalViewer(url)));
          errors.add(errorResult);
        }
      }
    }
    return result;
  }
}
