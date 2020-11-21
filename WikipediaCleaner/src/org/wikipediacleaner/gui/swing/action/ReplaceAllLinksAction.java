/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.action;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Collection;

import javax.swing.JOptionPane;

import org.wikipediacleaner.api.data.LinkReplacement;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.ilink.InternalLinkBuilder;
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
  @Override
  public void actionPerformed(@SuppressWarnings("unused") ActionEvent e) {
    if ((warning != null) && (warning.trim().length() > 0)) {
      String message = warning + "\n" + GT._T("Do you want to proceed with the replacement ?");
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
        buffer.append(InternalLinkBuilder.from(to).withText(link.getDisplayedText()).toString());
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
