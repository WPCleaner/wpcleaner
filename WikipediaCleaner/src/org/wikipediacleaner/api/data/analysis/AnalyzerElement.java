/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2018  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data.analysis;

import java.util.ArrayList;
import java.util.List;

import org.wikipediacleaner.api.data.contents.ContentsElement;


/**
 * Base class for analyzing contents for a given type of element.
 */
abstract class AnalyzerElement {

  /** Types of elements managed by the analyzer */
  private final List<Class> managedClasses;

  /**
   * Creation of a content analyzer.
   */
  AnalyzerElement(List<Class> managedClasses) {
    this.managedClasses = (managedClasses != null) ? managedClasses : new ArrayList<>();
  }

  /**
   * @param elementsClass Type of element.
   * @return True if this analyzer can handler the provided type of element.
   */
  boolean handleAnalysis(Class<? extends ContentsElement> elementsClass) {
    return managedClasses.contains(elementsClass);
  }

  /**
   * @param contents Contents to be analyzed.
   */
  abstract void analyze(Contents contents);
}
