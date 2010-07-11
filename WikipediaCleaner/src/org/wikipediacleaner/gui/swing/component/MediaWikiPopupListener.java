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

package org.wikipediacleaner.gui.swing.component;

import java.awt.Rectangle;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.JCheckBox;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JSeparator;
import javax.swing.JTextPane;
import javax.swing.text.BadLocationException;
import javax.swing.text.Element;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;


/**
 * A popup menu listener for MediaWikiPane. 
 */
public class MediaWikiPopupListener implements MouseListener, KeyListener {

  private final EnumWikipedia wikipedia;
  private JCheckBox chkAddNote;
  private BasicWindow window;

  public MediaWikiPopupListener(EnumWikipedia wikipedia, BasicWindow window) {
    this.wikipedia = wikipedia;
    this.window = window;
  }

  /**
   * @param chk CheckBox used to request adding a note on talk page.
   */
  public void setCheckBoxAddNote(JCheckBox chk) {
    chkAddNote = chk;
  }

  /**
   * Construct and show popup menu if necessary.
   * 
   * @param e Event.
   */
  private void maybeShowPopup(MouseEvent e) {

    // Retrieve informations
    if (!e.isPopupTrigger()) {
      return;
    }
    if (!(e.getComponent() instanceof JTextPane)) {
      return;
    }
    JTextPane textPane = (JTextPane) e.getComponent();
    showPopup(textPane, textPane.viewToModel(e.getPoint()), e.getX(), e.getY());
  }

  /**
   * Construct and show popup menu if necessary.
   * 
   * @param e Event.
   */
  private void maybeShowPopup(KeyEvent e) {

    // Retrieve informations
    if (e.getKeyCode() != KeyEvent.VK_CONTEXT_MENU) {
      return;
    }
    if (!(e.getComponent() instanceof JTextPane)) {
      return;
    }
    JTextPane textPane = (JTextPane) e.getComponent();
    try {
      Rectangle rect = textPane.modelToView(textPane.getCaretPosition());
      showPopup(textPane, textPane.getSelectionStart(), rect.x, rect.y);
    } catch (BadLocationException e1) {
      //
    }
  }

  /**
   * Construct and show popup menu.
   * 
   * @param textPane Text pane.
   * @param position Position in text.
   * @param x Position.
   * @param y Position.
   */
  private void showPopup(JTextPane textPane, int position, int x, int y) {
    Element element = textPane.getStyledDocument().getCharacterElement(position);
    if (element == null) {
      return;
    }

    // Manage Informations
    Object attrInfo = element.getAttributes().getAttribute(MediaWikiConstants.ATTRIBUTE_INFO);
    if (attrInfo instanceof CheckErrorResult) { // TODO: more generic
      CheckErrorResult info = (CheckErrorResult) attrInfo;
      JPopupMenu popup = new JPopupMenu();
      MenuCreator.addInfoToMenu(popup, element, textPane, position, info);
      popup.show(textPane, x, y);
      return;
    }

    Object attrPage = element.getAttributes().getAttribute(MediaWikiConstants.ATTRIBUTE_PAGE);
    Object attrText = element.getAttributes().getAttribute(MediaWikiConstants.ATTRIBUTE_TEXT);
    if (!(attrPage instanceof Page)) {

      // Trying to find if the click has been made in an internal link [[...]]
      try {
        String text = element.getDocument().getText(
            element.getStartOffset(),
            element.getEndOffset() - element.getStartOffset());
        int localOffset = position - element.getStartOffset();
        int previousStartLink = text.lastIndexOf("[[", localOffset);
        int previousEndLink = text.lastIndexOf("]]", localOffset);
        int nextStartLink = text.indexOf("[[", localOffset);
        int nextEndLink = text.indexOf("]]", localOffset);
        if ((previousStartLink != -1) && (nextEndLink != -1)) {
          if ((previousEndLink == -1) || (previousEndLink < previousStartLink)) {
            if ((nextStartLink == -1) || (nextStartLink > nextEndLink)) {
              String foundText = text.substring(previousStartLink + 2, nextEndLink);
              int separator = foundText.indexOf('|');
              if (separator != -1) {
                foundText = foundText.substring(0, separator);
              }
              attrPage = DataManager.getPage(wikipedia, foundText, null, null);
            }
          }
        }
      } catch (BadLocationException e) {
        //
      }
      attrText = null;
    }
    if ((!(attrPage instanceof Page)) || (!(attrText instanceof String))) {

      // Trying to find if the click has been made in an internal [[...]] or external link [http://...]
      try {
        String text = element.getDocument().getText(
            element.getStartOffset(),
            element.getEndOffset() - element.getStartOffset());
        int localOffset = position - element.getStartOffset();
        int previousStartLink = text.lastIndexOf("[", localOffset);
        int previousEndLink = text.lastIndexOf("]", localOffset);
        int nextStartLink = text.indexOf("[", localOffset);
        int nextEndLink = text.indexOf("]", localOffset);
        if ((previousStartLink != -1) && (nextEndLink != -1)) {
          if ((previousEndLink == -1) || (previousEndLink < previousStartLink)) {
            if ((nextStartLink == -1) || (nextStartLink > nextEndLink)) {
              String foundText = text.substring(previousStartLink + 1, nextEndLink);
              if ((previousStartLink > 0) &&
                  (text.charAt(previousStartLink) == '[') &&
                  (text.charAt(previousStartLink - 1) == '[') &&
                  (nextEndLink + 1 < text.length()) &&
                  (text.charAt(nextEndLink) == ']') &&
                  (text.charAt(nextEndLink + 1) == ']')) {
                int separator = foundText.indexOf('|');
                if (separator != -1) {
                  foundText = foundText.substring(0, separator);
                }
                if (foundText.length() > 0) {
                  JPopupMenu popup = new JPopupMenu();
                  MenuCreator.addCurrentChapterToMenu(popup, textPane, position);
                  MenuCreator.addViewToMenu(wikipedia, popup, foundText);
                  popup.show(textPane, x, y);
                  return;
                }
              }
              int separator = foundText.indexOf(' ');
              if (separator != -1) {
                foundText = foundText.substring(0, separator);
              }
              if (foundText.length() > 0) {
                JPopupMenu popup = new JPopupMenu();
                MenuCreator.addCurrentChapterToMenu(popup, textPane, position);
                MenuCreator.addViewToMenu(null, popup, foundText);
                popup.show(textPane, x, y);
                return;
              }
            }
          }
        }
      } catch (BadLocationException e) {
        //
      }

      // Trying to find if the click has been made in a template {{...}}
      try {
        String text = element.getDocument().getText(
            element.getStartOffset(),
            element.getEndOffset() - element.getStartOffset());
        int localOffset = position - element.getStartOffset();
        int previousStartLink = text.lastIndexOf("{{", localOffset);
        int previousEndLink = text.lastIndexOf("}}", localOffset);
        int nextStartLink = text.indexOf("{{", localOffset);
        int nextEndLink = text.indexOf("}}", localOffset);
        if ((previousStartLink != -1) && (nextEndLink != -1)) {
          if ((previousEndLink == -1) || (previousEndLink < previousStartLink)) {
            if ((nextStartLink == -1) || (nextStartLink > nextEndLink)) {
              String foundText = text.substring(previousStartLink + 2, nextEndLink);
              int separatorIndex = foundText.indexOf("|");
              if (separatorIndex == -1) {
                separatorIndex = foundText.length();
              }
              if (foundText.length() > 0) {
                String template = foundText.substring(0, separatorIndex);
                String title = "Template:" + template;
                String params = (separatorIndex < foundText.length()) ?
                    foundText.substring(separatorIndex + 1) : "";
                Page page = DataManager.getPage(wikipedia, title, null, null);
                JPopupMenu popup = new JPopupMenu();
                JMenuItem menuItem = new JMenuItem(page.getTitle());
                menuItem.setEnabled(false);
                popup.add(menuItem);
                MenuCreator.addCurrentChapterToMenu(popup, textPane, position);
                if (attrPage instanceof Page) {
                  popup.add(new JSeparator());
                  MenuCreator.addReplaceTemplateToMenu(
                      wikipedia, popup, template, params, (Page) attrPage,
                      null /*TODO*/, element, textPane);
                  MenuCreator.addAnalyzeToMenu(wikipedia, popup, (Page) attrPage);
                  MenuCreator.addViewToMenu(wikipedia, popup, (Page) attrPage);
                  MenuCreator.addDisambiguationToMenu(wikipedia, popup, (Page) attrPage);
                  MenuCreator.addReloadLinksToMenu(wikipedia, popup, (Page) attrPage, window);
                } else {
                  MenuCreator.addAnalyzeToMenu(wikipedia, popup, page);
                  MenuCreator.addViewToMenu(wikipedia, popup, page);
                }
                popup.show(textPane, x, y);
                return;
              }
            }
          }
        }
      } catch (BadLocationException e) {
        System.err.println("BadLocationException: " + e.getMessage());
      }
      return;
    }

    // Menu name
    Page page = (Page) attrPage;
    String text = (String) attrText;
    JPopupMenu popup = new JPopupMenu();
    JMenuItem menuItem = new JMenuItem(page.getTitle());
    menuItem.setEnabled(false);
    popup.add(menuItem);
    MenuCreator.addCurrentChapterToMenu(popup, textPane, position);
    popup.add(new JSeparator());

    // Create sub menus
    MenuCreator.addReplaceLinkToMenu(popup, page, text, element, textPane);
    MenuCreator.addRemoveLinkToMenu(popup, text, element, textPane);
    MenuCreator.addMarkAsNormalToMenu(wikipedia, popup, page, text, element, textPane);
    MenuCreator.addMarkAsNeedingHelpToMenu(wikipedia, popup, page, text, element, textPane, chkAddNote);
    MenuCreator.addLinkTextToMenu(wikipedia, popup, page, text, element, textPane);
    popup.add(new JSeparator());
    MenuCreator.addAnalyzeToMenu(wikipedia, popup, page);
    MenuCreator.addViewToMenu(wikipedia, popup, page);
    MenuCreator.addDisambiguationToMenu(wikipedia, popup, page);
    MenuCreator.addReloadLinksToMenu(wikipedia, popup, page, window);

    popup.show(textPane, x, y);
  }

  /* --------------------------------------------------------------------- */
  /* MouseListener implementation                                          */
  /* --------------------------------------------------------------------- */

  /* (non-Javadoc)
   * @see java.awt.event.MouseAdapter#mouseClicked(java.awt.event.MouseEvent)
   */
  public void mouseClicked(MouseEvent e) {
    maybeShowPopup(e);
  }

  /* (non-Javadoc)
   * @see java.awt.event.MouseAdapter#mousePressed(java.awt.event.MouseEvent)
   */
  public void mousePressed(MouseEvent e) {
    maybeShowPopup(e);
  }

  /* (non-Javadoc)
   * @see java.awt.event.MouseAdapter#mouseReleased(java.awt.event.MouseEvent)
   */
  public void mouseReleased(MouseEvent e) {
    maybeShowPopup(e);
  }

  /* (non-Javadoc)
   * @see java.awt.event.MouseListener#mouseEntered(java.awt.event.MouseEvent)
   */
  public void mouseEntered(@SuppressWarnings("unused") MouseEvent e) {
    //
  }

  /* (non-Javadoc)
   * @see java.awt.event.MouseListener#mouseExited(java.awt.event.MouseEvent)
   */
  public void mouseExited(@SuppressWarnings("unused") MouseEvent e) {
    //
  }

  /* --------------------------------------------------------------------- */
  /* KeyListener implementation                                            */
  /* --------------------------------------------------------------------- */

  /* (non-Javadoc)
   * @see java.awt.event.KeyListener#keyPressed(java.awt.event.KeyEvent)
   */
  public void keyPressed(@SuppressWarnings("unused") KeyEvent e) {
    //
  }

  /* (non-Javadoc)
   * @see java.awt.event.KeyListener#keyReleased(java.awt.event.KeyEvent)
   */
  public void keyReleased(KeyEvent e) {
    maybeShowPopup(e);
  }

  /* (non-Javadoc)
   * @see java.awt.event.KeyListener#keyTyped(java.awt.event.KeyEvent)
   */
  public void keyTyped(@SuppressWarnings("unused") KeyEvent e) {
    //
  }
}
