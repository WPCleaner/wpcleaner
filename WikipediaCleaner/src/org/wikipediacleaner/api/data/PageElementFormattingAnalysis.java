/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2018  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Bean for storing the analysis of formatting elements in an area.
 */
public class PageElementFormattingAnalysis {

  /** List of formatting elements */
  private final List<PageElementFormatting> elements;

  /** Count of bold formatting */
  private  final int boldCount;

  /** Count of italic formatting */
  private final int italicCount;

  /** Static object for empty analysis to avoid useless memory allocation */
  final private static PageElementFormattingAnalysis EMPTY = new PageElementFormattingAnalysis(new ArrayList<>(), 0, 0);

  /**
   * @param bold Count of bold formatting.
   * @param italic Count of italic formatting.
   */
  private PageElementFormattingAnalysis(
      List<PageElementFormatting> elements,
      int bold, int italic) {
    this.elements = elements;
    this.boldCount = bold;
    this.italicCount = italic;
  }

  /**
   * @return Count of bold formatting.
   */
  public int getBoldCount() {
    return boldCount;
  }

  /**
   * @return Count of italic formatting.
   */
  public int getItalicCount() {
    return italicCount;
  }

  /**
   * @return List of formatting elements.
   */
  public List<PageElementFormatting> getElements() {
    return Collections.unmodifiableList(elements);
  }

  /**
   * Analyze an area for its formatting elements.
   * 
   * @param elements Formatting elements.
   * @param beginIndex Begin index of the text area.
   * @param endIndex End index of the text area.
   * @return Analysis.
   */
  public static PageElementFormattingAnalysis analyzeArea(
      List<PageElementFormatting> elements,
      int beginIndex, int endIndex) {
    int bold = 0;
    int italic = 0;
    List<PageElementFormatting> selected = null;
    for (int index = 0; index < elements.size(); index++) {
      PageElementFormatting element = elements.get(index);
      if ((element.getIndex() >= beginIndex) &&
          (element.getIndex() + element.getLength() <= endIndex)) {
        if (selected == null) {
          selected = new ArrayList<>();
        }
        selected.add(element);
        if (element.isBold()) {
          bold++;
        }
        if (element.isItalic()) {
          italic++;
        }
      }
    }
    if (bold + italic == 0) {
      return PageElementFormattingAnalysis.EMPTY;
    }
    return new PageElementFormattingAnalysis(selected, bold, italic);
  }
}