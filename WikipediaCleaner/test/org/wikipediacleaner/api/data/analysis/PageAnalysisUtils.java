/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2020  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data.analysis;

import static org.junit.Assert.fail;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.charset.StandardCharsets;

import org.apache.commons.io.FileUtils;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Page;

/**
 * Utility class.
 */
public class PageAnalysisUtils {

  private PageAnalysisUtils() {
  }

  /**
   * Perform an analysis and some global tests.
   * 
   * @param wiki Wiki.
   * @param fileName File name.
   * @return Page analysis.
   */
  public static PageAnalysis analyzeAndTestPage(EnumWikipedia wiki, String fileName) {

    // Create contents and analysis
    String text = readFile(fileName + ".txt");
    Page testPage = DataManager.getPage(wiki, fileName, null, null, null);
    PageAnalysis analysis = new PageAnalysis(testPage, text);
    AnalysisPerformance perf = new AnalysisPerformance();
    analysis.performFullPageAnalysis(perf);

    // Display performance
    System.out.println("Page analysis of " + fileName + ": " + perf.toMilliSeconds());

    return analysis;
  }

  /**
   * Read a test file.
   * 
   * @param fileName File name.
   * @return Contents of the test file.
   */
  private static String readFile(String fileName) {
    File testFile = new File("test/org/wikipediacleaner/api/data/analysis/" + fileName);
    try {
      return FileUtils.readFileToString(testFile, StandardCharsets.UTF_8);
    } catch (FileNotFoundException e) {
      fail("Unable to open test file: " + testFile.getAbsolutePath());
    } catch (IOException e) {
      fail("Error reading file: " + testFile + "\n" + e.getMessage());
    }
    return null;
  }
}
