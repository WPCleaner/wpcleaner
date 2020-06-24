/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.component;

import javax.swing.text.StyledDocument;

import org.wikipediacleaner.api.data.analysis.PageAnalysis;


/**
 * A basic formatter for MediaWikiPane.
 */
public class MWPaneBasicFormatter extends MWPaneFormatter {

  /**
   * Format text in a StyleDocument.
   * 
   * @param doc Document to be formatted.
   * @param pageAnalysis Page analysis.
   */
  @Override
  public void format(StyledDocument doc, PageAnalysis pageAnalysis) {
    // Clean formatting
    cleanFormat(doc);

    // Format comments
    defaultFormatElements(doc, pageAnalysis);
  }

  /**
   * Move caret.
   * 
   * @param pane MediaWikiPane.
   */
  @Override
  protected void moveCaret(MWPane pane) {
    //
  }
}
