/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2007  Nicolas Vervelle
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.wikipediacleaner.gui.swing.action;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.AbstractButton;

import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
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
  public void actionPerformed(@SuppressWarnings("unused") ActionEvent e) {
    String contents = textComponent.getText();
    PageAnalysis analysis = new PageAnalysis(page, contents);
    contents = algorithm.fix(fixName, analysis, textComponent);
    textComponent.setText(contents);
    if (button != null) {
      button.doClick();
      textComponent.setModified(true);
    }
  }
}
