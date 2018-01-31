/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2018  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data.contents;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;


/**
 * Analyzer for the page contents.
 */
public class ContentsAnalyzer {

  /** Contents */
  private final Contents contents;

  /** Lock for concurrent access */
  private final Object lock = new Object();

  /** List of analyzers that have not been run yet */
  private final List<ContentsElementAnalyzer> analyzersToDo;

  /** List of analyzers that have already been run */
  private final List<ContentsElementAnalyzer> analyzersDone;

  /**
   * @param contents Contents.
   */
  ContentsAnalyzer(Contents contents) {
    this.contents = contents;
    analyzersToDo = new ArrayList<>();
    analyzersToDo.add(new ContentsCommentAnalyzer());
    analyzersDone = new ArrayList<>();
  }

  /**
   * @param elementsClass Type of element to analyze.
   */
  void analyze(Class<? extends ContentsElement> elementsClass) {

    synchronized (lock) {
      // Check if the analysis has already been done
      for (ContentsElementAnalyzer analyzer : analyzersDone) {
        if (analyzer.handleAnalysis(elementsClass)) {
          return;
        }
      }
  
      // Check that the analysis can be done
      boolean handled = false;
      for (ContentsElementAnalyzer analyzer : analyzersToDo) {
        if (analyzer.handleAnalysis(elementsClass)) {
          handled = true;
        }
      }
      if (!handled) {
        return;
      }
  
      // Do the analysis
      Iterator<? extends ContentsElementAnalyzer> itAnalyzer = analyzersToDo.iterator();
      while (itAnalyzer.hasNext()) {
        ContentsElementAnalyzer analyzer = itAnalyzer.next();
        handled = analyzer.handleAnalysis(elementsClass);
        analyzer.analyze(contents);
        analyzersDone.add(analyzer);
        itAnalyzer.remove();
      }
    }
  }
}
