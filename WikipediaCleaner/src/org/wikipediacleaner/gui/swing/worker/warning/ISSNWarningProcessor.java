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
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmISSN;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithms;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.PageElementISSN;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;

/**
 * Processor for disambiguation warnings on talk pages.
 */
public class ISSNWarningProcessor extends WarningProcessor {

  /**
   * Create a processor for disambiguation warnings on talk pages.
   * 
   * @param wiki Wiki.
   */
  public ISSNWarningProcessor(final EnumWikipedia wiki) {
    super(wiki);
  }

  /**
   * Extract information about ISSN with errors.
   * 
   * @param analysis Page analysis (must have enough information to compute the list of ISSN errors).
   * @param talkPage Talk page.
   * @param todoSubpage to do sub-page.
   * @return List of ISSN errors.
   */
  @Override
  protected Collection<String> constructWarningElements(
      PageAnalysis analysis, Page talkPage, Page todoSubpage) {
    if ((analysis == null) || (analysis.getPage() == null)) {
      return null;
    }

    // Prepare list of algorithms
    List<CheckErrorAlgorithm> algorithms = new ArrayList<>();
    algorithms.add(CheckErrorAlgorithms.getAlgorithm(wiki, 106)); // Incorrect syntax
    algorithms.add(CheckErrorAlgorithms.getAlgorithm(wiki, 107)); // Wrong length
    algorithms.add(CheckErrorAlgorithms.getAlgorithm(wiki, 108)); // Wrong checksum

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
        PageElementISSN issn = analysis.isInISSN(beginIndex);
        if (issn != null) {
          if ((algorithm != null) &&
              (algorithm instanceof CheckErrorAlgorithmISSN)) {
            CheckErrorAlgorithmISSN issnAlgo = (CheckErrorAlgorithmISSN) algorithm;
            String reason = issnAlgo.getReason(issn);
            if ((reason != null) && (reason.length() > 0)) {
              if (comment.length() > 0) {
                comment.append(" - ");
              }
              comment.append(reason);
            }
          }
          if (!issn.isTemplateParameter()) {
            if (error.toUpperCase().startsWith("ISSN")) {
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
        elements.add(error);
        elements.add(comment.toString());
        memorizeError(error, analysis.getPage().getTitle());
      }
    }
    return elements;
  }
}
