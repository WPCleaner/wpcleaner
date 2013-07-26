/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
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

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;

import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.ListCellRenderer;
import javax.swing.border.EmptyBorder;

import org.wikipediacleaner.api.check.CheckErrorPage;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.images.EnumImageSize;


/**
 * A renderer for CheckErrorAlgorithm items in a list.
 */
public class CheckErrorPageListCellRenderer extends JLabel implements ListCellRenderer {

  private static final long serialVersionUID = 1L;

  private final boolean forPage;
  private boolean showCountOccurence;
  private ImageIcon globalFixIcon;

  /**
   * Construct a renderer.
   * 
   * @param forPage Renderer for page or for algorithm ?
   */
  public CheckErrorPageListCellRenderer(boolean forPage) {
    this.forPage = forPage;
    setOpaque(true);
    setHorizontalAlignment(LEFT);
    setVerticalAlignment(CENTER);
    setBorder(new EmptyBorder(0, 3, 0, 3));
    setFont(getFont().deriveFont(Font.PLAIN));
    globalFixIcon = Utilities.getImageIcon("gnome-system-run.png", EnumImageSize.SMALL);
  }

  /**
   * @param show Flag indicating the occurence count is shown.
   */
  public void showCountOccurence(boolean show) {
    showCountOccurence = show;
  }

  /* (non-Javadoc)
   * @see javax.swing.ListCellRenderer#getListCellRendererComponent(
   *          javax.swing.JList, java.lang.Object, int, boolean, boolean)
   */
  public Component getListCellRendererComponent(
      JList list,
      Object value,
      @SuppressWarnings("unused") int index,
      boolean isSelected,
      @SuppressWarnings("unused") boolean cellHasFocus) {

    // Retrieve data
    String text = (value != null) ? value.toString() : "";
    Boolean errorsPresent = null;
    Boolean globalFix = null;
    boolean whiteList = false;
    if (value instanceof CheckErrorPage) {
      CheckErrorPage errorPage = (CheckErrorPage) value;
      whiteList = errorPage.isInWhiteList();
      if (forPage && (errorPage.getPage() != null)) {
        text = errorPage.getPage().getTitle();
      } else {
        text = errorPage.getAlgorithm().toString();
        int errorCount = errorPage.getActiveResultsCount();
        if (errorCount > 0) {
          errorsPresent = Boolean.TRUE;
          if ((showCountOccurence) &&
              (errorCount > 1)) {
            text += " (" + errorCount + ")";
          }
        } else if (errorPage.getErrorFound()) {
          errorsPresent = Boolean.TRUE;
        } else {
          errorsPresent = Boolean.FALSE;
        }
        String[] globalFixes = errorPage.getAlgorithm().getGlobalFixes();
        if ((globalFixes != null) && (globalFixes.length > 0)) {
          globalFix = Boolean.TRUE;
        }
      }
    }

    // Text
    setText(text);
    if (Boolean.TRUE.equals(globalFix)) {
      setIcon(globalFixIcon);
    } else {
      setIcon(null);
    }

    // Color
    Color background = isSelected ? list.getSelectionBackground() : Color.WHITE;
    Color foreground = isSelected ? list.getSelectionForeground() : list.getForeground();
    if (forPage) {
      if (whiteList) {
        foreground = Color.GREEN;
      }
    } else if (errorsPresent == null) {
      if (!isSelected) {
        foreground = Color.DARK_GRAY;
      }
    } else if (whiteList) {
      foreground = Color.GREEN;
    } else if (errorsPresent.booleanValue()) {
      foreground = Color.RED;
    }
    setBackground(background);
    setForeground(foreground);

    return this;
  }

}
