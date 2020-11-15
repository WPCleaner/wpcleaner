/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.constants.ArticleUrl;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.ilink.ContentsInternalLinkBuilder;


/**
 * Algorithm for analyzing error 004 of check wikipedia project.
 * Error 004: &lt;a&gt; tags in main namespace
 */
public class CheckErrorAlgorithm004 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm004() {
    super("<a> tags");
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
    if ((analysis == null) || (analysis.getPage() == null)) {
      return false;
    }
    Integer ns = analysis.getPage().getNamespace();
    if ((ns == null) || (ns.intValue() != Namespace.MAIN)) {
      return false;
    }

    // Check each tag
    List<PageElementTag> tags = analysis.getTags(PageElementTag.TAG_HTML_A);
    if ((tags == null) || (tags.isEmpty())) {
      return false;
    }
    boolean result = false;
    for (PageElementTag tag : tags) {
      boolean shouldKeep = true;

      if (!tag.isFullTag() && tag.isEndTag() && tag.isComplete()) {
        shouldKeep = false;
      }

      int index = tag.getBeginIndex();
      if ((analysis.getSurroundingTag(PageElementTag.TAG_HTML_CODE, index) != null) ||
          (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_IMAGEMAP, index) != null) ||
          (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_MATH, index) != null) ||
          (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_MATH_CHEM, index) != null) ||
          (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_NOWIKI, index) != null) ||
          (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_PRE, index) != null) ||
          (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_SCORE, index) != null) ||
          (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_SOURCE, index) != null) ||
          (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_SYNTAXHIGHLIGHT, index) != null)) {
        shouldKeep = false;
      }

      if (shouldKeep) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis, tag.getCompleteBeginIndex(), tag.getCompleteEndIndex());
        if (tag.isFullTag()) {
          errorResult.addReplacement("");
        } else if (tag.isComplete()) {
          String internalText = analysis.getContents().substring(
              tag.getValueBeginIndex(), tag.getValueEndIndex());
          PageElementTag.Parameter href = tag.getParameter("href");
          String hrefValue = (href != null) ?href.getTrimmedValue() : null;
  
          // Check for link with "tel:" protocol as href
          if ((hrefValue != null) &&
              (hrefValue.startsWith("tel:"))) {
            errorResult.addReplacement(internalText, true);
          }
  
          // Check for link with internal link as href
          if (hrefValue != null) {
            ArticleUrl articleUrl = ArticleUrl.isArticleUrl(analysis.getWikipedia(), hrefValue);
            if (articleUrl != null) {
              String article = articleUrl.getTitle();
              if ((article != null) && (article.length() > 0)) {
                boolean automatic = true;
                if ((articleUrl.getAttributes() != null) ||
                    (articleUrl.getFragment() != null)) {
                  automatic = false;
                }
                errorResult.addReplacement(
                    ContentsInternalLinkBuilder.from(article).withText(internalText).toString(),
                    automatic);
              }
            }
          }
  
          // Check for link
          if ((hrefValue != null) && (hrefValue.length() > 0)) {
            boolean protocolOk = PageElementExternalLink.isPossibleProtocol(hrefValue, 0);
            if (protocolOk) {
              errorResult.addReplacement(
                  PageElementExternalLink.createExternalLink(hrefValue, internalText));
            }
          }
  
          errorResult.addReplacement(internalText);
        } else {
          errorResult.addReplacement("");
        }
        errors.add(errorResult);
      }
    }

    return result;
  }

  /**
   * Automatic fixing of some errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalAutomaticFix(PageAnalysis analysis) {
    return fixUsingAutomaticReplacement(analysis);
  }
}
