/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2020  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm;

import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.analysis.PageAnalysisUtils;

/**
 * Test class for CW algorithms.
 */
public class CheckErrorAlgorithmTest {

  /**
   * Test performance of an algorithm.
   */
  public void testPerformance(
      EnumWikipedia wiki,
      String fileName,
      int algorithmNumber,
      int times,
      int max) {

    // Create contents and analysis
    PageAnalysis analysis = PageAnalysisUtils.analyzeAndTestPage(wiki, fileName);
    CheckErrorAlgorithms.initializeAlgorithms(wiki);
    CheckErrorAlgorithm algorithm = CheckErrorAlgorithms.getAlgorithm(wiki, algorithmNumber);
    List<CheckErrorResult> errors = new ArrayList<>();
    long timeSpent = 0;
    for (int checkNum = 0; checkNum < times; checkNum++) {
      errors.clear();
      long begin = System.nanoTime();
      algorithm.analyze(analysis, errors, false);
      long end = System.nanoTime();
      timeSpent += (end - begin);
    }
    timeSpent /= 1000000;
    System.out.println("Algorithm " + algorithmNumber + " on " + fileName + "(x" + times + "): " + timeSpent + " ms");
    assertTrue("Algorithm " + algorithmNumber + " needs performance improvements", timeSpent <= max);
  }

}
