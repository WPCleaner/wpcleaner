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
import java.awt.event.ActionListener;
import java.util.Collection;

import javax.swing.JOptionPane;

import org.wikipediacleaner.api.data.LinkReplacement;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.i18n.GT;


/**
 * An action listener for replacing all internal links.
 */
public class ReplaceAllLinksAction implements ActionListener {

  private final MWPane textPane;
  private final Page from;
  private final String to;
  private final String warning;

  /**
   * @param textPane MWPane on which the edition has to be done.
   * @param from Page on which links are currently pointing to.
   * @param to New destination.
   * @param warning Potential warning message.
   */
  public ReplaceAllLinksAction(MWPane textPane, Page from, String to, String warning) {
    this.textPane = textPane;
    this.from = from;
    this.to = to;
    this.warning = warning;
  }

  /* (non-Javadoc)
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  public void actionPerformed(@SuppressWarnings("unused") ActionEvent e) {
    if ((warning != null) && (warning.trim() != null)) {
      String message = warning + "\n" + GT._("Do you want to proceed with the replacement ?");
      int answer = Utilities.displayYesNoWarning(textPane.getParent(), message);
      if (answer != JOptionPane.YES_OPTION) {
        return;
      }
    }
    String originalText = textPane.getText();
    PageAnalysis analysis = textPane.getWikiPage().getAnalysis(originalText, true);
    StringBuilder buffer = new StringBuilder();
    int lastPosition = 0;
    Collection<PageElementInternalLink> links = analysis.getInternalLinks();
    for (PageElementInternalLink link : links) {
      if (Page.areSameTitle(from.getTitle(), link.getLink())) {
        buffer.append(originalText.substring(lastPosition, link.getBeginIndex()));
        lastPosition = link.getBeginIndex();
        buffer.append(PageElementInternalLink.createInternalLink(to, link.getDisplayedText()));
        lastPosition = link.getEndIndex();
      }
    }
    if (lastPosition > 0) {
      buffer.append(originalText.substring(lastPosition));
      textPane.changeText(buffer.toString());
    }
    LinkReplacement.addLastReplacement(from.getTitle(), to);
  }
}
