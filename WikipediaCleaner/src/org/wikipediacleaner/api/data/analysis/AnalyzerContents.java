/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2018  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data.analysis;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.wikipediacleaner.api.data.contents.ContentsElement;


/**
 * Analyzer for the page contents.
 */
public class AnalyzerContents {

  /** Contents */
  private final Contents contents;

  /** Lock for concurrent access */
  private final Object lock = new Object();

  /** List of analyzers that have not been run yet */
  private final List<AnalyzerElement> analyzersToDo;

  /** List of analyzers that have already been run */
  private final List<AnalyzerElement> analyzersDone;

  /**
   * @param contents Contents.
   */
  AnalyzerContents(Contents contents) {
    this.contents = contents;
    analyzersToDo = new ArrayList<>();
    analyzersToDo.add(new AnalyzerComment());
    analyzersDone = new ArrayList<>();
  }

  /**
   * @param elementsClass Type of element to analyze.
   */
  void analyze(Class<? extends ContentsElement> elementsClass) {

    synchronized (lock) {
      // Check if the analysis has already been done
      for (AnalyzerElement analyzer : analyzersDone) {
        if (analyzer.handleAnalysis(elementsClass)) {
          return;
        }
      }
  
      // Check that the analysis can be done
      boolean handled = false;
      for (AnalyzerElement analyzer : analyzersToDo) {
        if (analyzer.handleAnalysis(elementsClass)) {
          handled = true;
        }
      }
      if (!handled) {
        return;
      }
  
      // Do the analysis
      Iterator<? extends AnalyzerElement> itAnalyzer = analyzersToDo.iterator();
      while (itAnalyzer.hasNext()) {
        AnalyzerElement analyzer = itAnalyzer.next();
        handled = analyzer.handleAnalysis(elementsClass);
        analyzer.analyze(contents);
        analyzersDone.add(analyzer);
        itAnalyzer.remove();
      }
    }
  }
}
