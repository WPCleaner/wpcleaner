/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.action;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.AbstractButton;

import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.gui.swing.component.MWPane;


/**
 * An action listener for global fixes of check error. 
 */
@SuppressWarnings("serial")
public class CheckErrorGlobalFixAction extends AbstractAction {

  private final CheckErrorAlgorithm algorithm;
  private final String fixName;
  private final Page page;
  private final MWPane textComponent;
  private final AbstractButton button;

  /**
   * @param algorithm Algorithm.
   * @param fixName Fix name.
   * @param page Page.
   * @param textComponent Text component containing the page text.
   * @param button Button to click on.
   */
  public CheckErrorGlobalFixAction(
      CheckErrorAlgorithm algorithm,
      String fixName,
      Page page,
      MWPane textComponent,
      AbstractButton button) {
    this.algorithm = algorithm;
    this.fixName = fixName;
    this.page = page;
    this.textComponent = textComponent;
    this.button = button;
  }

  /* (non-Javadoc)
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed(@SuppressWarnings("unused") ActionEvent e) {
    String contents = textComponent.getText();
    PageAnalysis analysis = page.getAnalysis(contents, true);
    contents = algorithm.fix(fixName, analysis, textComponent);
    textComponent.setText(contents);
    if (button != null) {
      button.doClick();
      textComponent.setModified(true);
    }
  }
}
