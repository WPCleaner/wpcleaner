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

import org.wikipediacleaner.api.check.AddInternalLinkActionProvider;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.constants.ArticleUrl;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.MagicWord;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.PageElementImage;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementImage.Parameter;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementTemplate;
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
    Namespace imageNamespace = analysis.getWikiConfiguration().getNamespace(Namespace.IMAGE);

    // Analyze each external link
    boolean result = false;
    List<PageElementExternalLink> links = analysis.getExternalLinks();
    if (links == null) {
      return result;
    }
    EnumWikipedia wiki = analysis.getWikipedia();
    String contents = analysis.getContents();
    for (PageElementExternalLink link : links) {
      ArticleUrl articleUrl = ArticleUrl.isArticleUrl(wiki, link.getLink());
      if (articleUrl != null) {
        if (errors == null) {
          return true;
        }
        result = true;
        boolean errorReported = false;
        String article = articleUrl.getTitle();

        // Check if link is in image as a link attribute
        PageElementImage image = analysis.isInImage(link.getBeginIndex());
        if ((image != null) && (!errorReported)) {
          Parameter imgParameter = image.getParameter(MagicWord.IMG_LINK);
          if (imgParameter != null) {
            int beginParam = image.getBeginIndex() + imgParameter.getBeginOffset();
            int endParam = image.getBeginIndex() + imgParameter.getEndOffset();
            if ((beginParam < link.getBeginIndex()) &&
                (endParam > link.getBeginIndex())) {

              // Decide if replacement can be automatic
              boolean automatic = true;
              if ((articleUrl.getAttributes() != null) ||
                  (articleUrl.getFragment() != null)) {
                automatic = false;
              }
              if (link.hasSquare()) {
                automatic = false;
              }
              int colonIndex = article.indexOf(':');
              if ((colonIndex <= 0) ||
                  (!imageNamespace.isPossibleName(article.substring(0, colonIndex).trim())) ||
                  (!Page.areSameTitle(article.substring(colonIndex + 1), image.getImage()))) {
                automatic = false;
              }

              // Report error
              CheckErrorResult errorResult = createCheckErrorResult(
                  analysis, beginParam - 1, endParam);
              errorResult.addReplacement("", automatic);
              errors.add(errorResult);
              errorReported = true;
            }
          }
        }

        // Retrieve information about link
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

        // Check if link is in template
        boolean isInTemplate = false;
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
                isInTemplate = true;
              }
            }
          }
        }

        // Restrict automatic modifications
        boolean automatic = !isInTemplate;
        if (Page.areSameTitle(article, analysis.getPage().getTitle())) {
          automatic = false;
        }
        if (articleUrl.getAttributes() != null) {
          for (Map.Entry<String, String> attribute : articleUrl.getAttributes().entrySet()) {
            if ("venotify".equals(attribute.getKey())) {
              if (!"created".equals(attribute.getValue())) {
                automatic = false;
              }
            } else if ("action".equals(attribute.getKey())) {
              if (!"edit".equals(attribute.getValue())) {
                automatic = false;
              }
            } else if (!"redlink".equals(attribute.getKey())) {
              automatic = false;
            }
          }
        }

        // Check if link is in a timeline tag
        if (!errorReported) {
          PageElementTag timelineTag = analysis.getSurroundingTag(PageElementTag.TAG_WIKI_TIMELINE, beginIndex);
          if ((timelineTag != null) && !isInTemplate) {
            automatic = false;

            boolean timelineOk = true;
            int timelineEnd = endIndex;
            if (contents.charAt(timelineEnd) == '\"') {
              timelineEnd++;
            }
            int timelineBegin = beginIndex;
            if (contents.charAt(timelineBegin - 1) == '\"') {
              timelineBegin--;
            }
            while ((timelineBegin > 0) && (contents.charAt(timelineBegin - 1) == ' ')) {
              timelineBegin--;
            }
            if (contents.startsWith("link:", timelineBegin - "link:".length())) {
              timelineBegin -= "link:".length();
            } else {
              timelineOk = false;
            }
            while ((timelineBegin > 0) && (contents.charAt(timelineBegin - 1) == ' ')) {
              timelineBegin--;
            }
            String displayedText = null;
            if (timelineBegin > 0) {
              if (contents.charAt(timelineBegin - 1) == '\"') {
                timelineBegin--;
                int tmpIndex = timelineBegin;
                while ((tmpIndex > 0) && ("\"\n".indexOf(contents.charAt(tmpIndex - 1)) < 0)) {
                  tmpIndex--;
                }
                if ((tmpIndex > 0) && (contents.charAt(tmpIndex - 1) == '\"')) {
                  displayedText = contents.substring(tmpIndex, timelineBegin);
                }
                timelineBegin = tmpIndex - 1;
              } else {
                int tmpIndex = timelineBegin;
                while ((tmpIndex > 0) && (" :".indexOf(contents.charAt(tmpIndex - 1)) < 0)) {
                  tmpIndex--;
                }
                if (tmpIndex > 0) {
                  displayedText = contents.substring(tmpIndex, timelineBegin);
                }
                timelineBegin = tmpIndex;
              }
            }
            if (displayedText == null) {
              timelineOk = false;
            }
            while ((timelineBegin > 0) && (contents.charAt(timelineBegin - 1) == ' ')) {
              timelineBegin--;
            }
            if (contents.startsWith("text:", timelineBegin - "text:".length())) {
              timelineBegin -= "text:".length();
            } else {
              timelineOk = false;
            }

            if (timelineOk) {
              CheckErrorResult errorResult = createCheckErrorResult(
                  analysis, timelineBegin, timelineEnd);
              errorResult.addReplacement(
                  "text:\"" +
                  PageElementInternalLink.createInternalLink(
                      article, articleUrl.getFragment(), displayedText) +
                  "\"");
              errors.add(errorResult);
              errorReported = true;
            }
          }
        }

        // Check if link is in a ref tag
        if (!errorReported) {
          PageElementTag refTag = analysis.getSurroundingTag(PageElementTag.TAG_WIKI_REF, beginIndex);
          if (refTag != null) {

            // Determine if the link is the full tag
            boolean full = true;
            int tmpIndex = beginIndex;
            while ((tmpIndex > refTag.getValueBeginIndex()) &&
                   (contents.charAt(tmpIndex - 1) == ' ')) {
              tmpIndex--;
            }
            if (tmpIndex > refTag.getValueBeginIndex()) {
              full = false;
            }
            tmpIndex = endIndex;
            while ((tmpIndex < refTag.getValueEndIndex()) &&
                   (contents.charAt(tmpIndex) == ' ')) {
              tmpIndex++;
            }
            if (tmpIndex < refTag.getValueEndIndex()) {
              full = false;
            }
            if (full) {
              automatic = false;
            }

            // Check if there's an equivalent link or text before
            tmpIndex = refTag.getCompleteBeginIndex();
            while ((tmpIndex > 0) && (contents.charAt(tmpIndex - 1) == ' ')) {
              tmpIndex--;
            }
            if (tmpIndex > 0) {
              PageElementInternalLink previousLink = analysis.isInInternalLink(tmpIndex - 1);
              if ((previousLink != null) && Page.areSameTitle(previousLink.getLink(), article)) {
                CheckErrorResult errorResult = createCheckErrorResult(
                    analysis, previousLink.getBeginIndex(), refTag.getCompleteEndIndex());
                errorResult.addReplacement(
                    contents.substring(previousLink.getBeginIndex(), previousLink.getEndIndex()));
                errors.add(errorResult);
                errorReported = true;
              } else if (tmpIndex > article.length()) {
                String textBefore = contents.substring(tmpIndex - article.length(), tmpIndex);
                if (Page.areSameTitle(article, textBefore)) {
                  CheckErrorResult errorResult = createCheckErrorResult(
                      analysis, tmpIndex - article.length(), refTag.getCompleteEndIndex());
                  errorResult.addReplacement(
                      PageElementInternalLink.createInternalLink(article, articleUrl.getFragment(), textBefore));
                  errors.add(errorResult);
                  errorReported = true;
                }
              }
            }
          }
        }

        // Link with text
        if (!errorReported && (text != null)) {
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, beginIndex, endIndex);
          errorResult.addReplacement(
              PageElementInternalLink.createInternalLink(
                  (needColon ? ":" : "") + articleUrl.getTitleAndFragment(), text),
              automatic);
          errors.add(errorResult);
          errorReported = true;
        }

        // Link without text but previous text
        if (!errorReported && (text == null)) {
          int tmpIndex = beginIndex;
          while ((tmpIndex > 0) && (contents.charAt(tmpIndex - 1) == ' ')) {
            tmpIndex--;
          }
          if (tmpIndex > article.length()) {
            String textBefore = contents.substring(tmpIndex - article.length(), tmpIndex);
            if (Page.areSameTitle(article, textBefore)) {
              CheckErrorResult errorResult = createCheckErrorResult(
                  analysis, tmpIndex - article.length(), endIndex);
              errorResult.addReplacement(
                  PageElementInternalLink.createInternalLink(
                      (needColon ? ":" : "") + articleUrl.getTitleAndFragment(), textBefore),
                  automatic);
              errors.add(errorResult);
              errorReported = true;
            }
          }
        }

        // Link without text
        if (!errorReported && (text == null)) {
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, beginIndex, endIndex);
          String question = GT._("What text should be displayed by the link?");
          AddInternalLinkActionProvider action = new AddInternalLinkActionProvider(
              article, articleUrl.getFragment(), null, null, null,
              question, articleUrl.getTitleAndFragment().replaceAll("\\_", " "), checker);
          errorResult.addPossibleAction(
              GT._("Convert into an internal link"),
              action);
          errors.add(errorResult);
          errorReported = true;
        }
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
