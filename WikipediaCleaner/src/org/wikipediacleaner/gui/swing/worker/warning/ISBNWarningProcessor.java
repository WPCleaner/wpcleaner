/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2022  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.worker.warning;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.wikipediacleaner.api.algorithm.Algorithm;
import org.wikipediacleaner.api.algorithm.AlgorithmError;
import org.wikipediacleaner.api.check.CheckErrorPage;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmISBN;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithms;
import org.wikipediacleaner.api.configuration.WPCConfigurationBoolean;
import org.wikipediacleaner.api.configuration.WPCConfigurationString;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.PageElementISBN;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.tag.CompleteTagBuilder;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;
import org.wikipediacleaner.i18n.GT;

/**
 * Processor for disambiguation warnings on talk pages.
 */
public class ISBNWarningProcessor extends WarningProcessor {

  /**
   * Create a processor for disambiguation warnings on talk pages.
   * 
   * @param wiki Wiki.
   */
  public ISBNWarningProcessor(final EnumWikipedia wiki) {
    super(wiki);
  }

  /**
   * Extract information about ISBN with errors.
   * 
   * @param analysis Page analysis (must have enough information to compute the list of ISBN errors).
   * @param talkPage Talk page.
   * @param todoSubpage to do sub-page.
   * @return List of ISBN errors.
   */
  @Override
  protected Collection<String> constructWarningElements(
      PageAnalysis analysis, Page talkPage, Page todoSubpage) {
    if ((analysis == null) || (analysis.getPage() == null)) {
      return null;
    }

    // Prepare list of algorithms
    List<CheckErrorAlgorithm> algorithms = new ArrayList<>();
    algorithms.add(CheckErrorAlgorithms.getAlgorithm(wiki, 69)); // Incorrect syntax
    algorithms.add(CheckErrorAlgorithms.getAlgorithm(wiki, 70)); // Incorrect length
    algorithms.add(CheckErrorAlgorithms.getAlgorithm(wiki, 71)); // Incorrect X
    algorithms.add(CheckErrorAlgorithms.getAlgorithm(wiki, 72)); // Incorrect ISBN-10
    algorithms.add(CheckErrorAlgorithms.getAlgorithm(wiki, 73)); // Incorrect ISBN-13

    // Retrieve list of errors
    List<CheckErrorResult> errorResults = new ArrayList<>();
    for (CheckErrorAlgorithm algorithm : algorithms) {
      int errorNumber = algorithm.getErrorNumber();
      if (CheckErrorAlgorithms.isAlgorithmActive(wiki, errorNumber) &&
          !algorithm.isInWhiteList(analysis.getPage().getTitle())) {
        CheckErrorPage errorPage = AlgorithmError.analyzeError(algorithm, analysis);
        List<CheckErrorResult> results = errorPage.getResults();
        if (results != null) {
          errorResults.addAll(results);
        }
      }
    }
    Collections.sort(errorResults);

    // Compute list of elements for the warning
    List<String> elements = new ArrayList<>();
    int pos = 0;
    while (pos < errorResults.size()) {
      CheckErrorResult errorResult = errorResults.get(pos);
      int beginIndex = errorResult.getStartPosition();
      int endIndex = errorResult.getEndPosition();
      int next = pos + 1;
      while ((next < errorResults.size()) &&
             (beginIndex == errorResults.get(next).getStartPosition()) &&
             (endIndex == errorResults.get(next).getEndPosition())) {
        next++;
      }
      String error = analysis.getContents().substring(beginIndex, endIndex);
      error = error.replaceAll("\\=", "&#x3D;"); // Replace "=" by its HTML value
      error = error.replaceAll("\n", "\u21b5"); // Replacer \n by a visual character
      error = error.replaceAll("\\<", "&lt;"); // Replace "<" by its HTML element
      error = error.replaceAll("\\[", "&#x5B;"); // Replace "[" by its HTML value
      error = error.replaceAll("\\]", "&#x5D;"); // Replace "]" by its HTML value
      error = error.replaceAll("\\{", "&#x7B;"); // Replace "{" by its HTML value
      error = error.replaceAll("\\|", "&#x7C;"); // Replace "|" by its HTML value
      error = error.replaceAll("\\}", "&#x7D;"); // Replace "}" by its HTML value
      boolean keep = true;
      StringBuilder comment = new StringBuilder();
      while (pos < next) {
        errorResult = errorResults.get(pos);
        Algorithm algorithm = errorResult.getAlgorithm();
        PageElementISBN isbn = analysis.isInISBN(beginIndex);
        if (isbn != null) {
          if ((algorithm != null) &&
              (algorithm instanceof CheckErrorAlgorithmISBN)) {
            CheckErrorAlgorithmISBN isbnAlgo = (CheckErrorAlgorithmISBN) algorithm;
            String reason = isbnAlgo.getReason(isbn);
            if ((reason != null) && (reason.length() > 0)) {
              if (comment.length() > 0) {
                comment.append(" - ");
              }
              comment.append(reason);
            }
          }
          if (!isbn.isTemplateParameter()) {
            if (error.toUpperCase().startsWith("ISBN")) {
              error = error.substring(4).trim();
            }
            PageElementExternalLink link = analysis.isInExternalLink(beginIndex);
            if (link != null) {
              if (!link.hasSquare() ||
                  (link.getText() == null) ||
                  link.getText().isEmpty()) {
                keep = false;
              } else if (beginIndex < link.getBeginIndex() + link.getLink().length()) {
                keep = false;
              }
            }
          }
        }
        pos++;
      }
      if (keep) {
        elements.add(CompleteTagBuilder.from(WikiTagType.NOWIKI, error).toString());
        elements.add(comment.toString());
        memorizeError(error, analysis.getPage().getTitle());
      }
    }
    return elements;
  }

  // ==========================================================================
  // Configuration
  // ==========================================================================

  /**
   * @return Configuration parameter for the warning template.
   */
  @Override
  protected WPCConfigurationString getWarningTemplate() {
    return WPCConfigurationString.ISBN_WARNING_TEMPLATE;
  }

  /**
   * @return Configuration parameter for the warning template comment.
   */
  @Override
  protected WPCConfigurationString getWarningTemplateComment() {
    return WPCConfigurationString.ISBN_WARNING_TEMPLATE_COMMENT;
  }

  /**
   * @return Configuration parameter telling if section 0 of the talk page should be used.
   */
  @Override
  protected WPCConfigurationBoolean getUseSection0() {
    return WPCConfigurationBoolean.ISBN_WARNING_SECTION_0;
  }

  /**
   * @return Comment when warning is removed.
   */
  @Override
  protected String getWarningCommentDone() {
    return configuration.getISBNWarningCommentDone();
  }

  /**
   * @param elements Message elements.
   * @return Comment when warning is added or updated.
   */
  @Override
  protected String getWarningComment(Collection<String> elements) {
    Collection<String> isbns = new ArrayList<>();
    int i = 0;
    for (String element : elements) {
      if (i % 2 == 0) {
        isbns.add(element);
      }
      i++;
    }
    return configuration.getISBNWarningComment(isbns);
  }

  /**
   * @param title Page title.
   * @return Message displayed when removing the warning from the page.
   */
  @Override
  protected String getMessageRemoveWarning(String title) {
    return GT._T("Removing {1} warning - {0}", new Object[] { title, "ISBN" });
  }

  /**
   * @param title Page title.
   * @return Message displayed when updating the warning from the page.
   */
  @Override
  protected String getMessageUpdateWarning(String title) {
    return GT._T("Updating {1} warning - {0}", new Object[] { title, "ISBN" });
  }
}
