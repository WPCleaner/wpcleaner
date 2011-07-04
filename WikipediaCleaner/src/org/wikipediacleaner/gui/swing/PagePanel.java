/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2011  Nicolas Vervelle
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

package org.wikipediacleaner.gui.swing;

import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.LayoutManager;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JToolBar;
import javax.swing.SwingConstants;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.component.MediaWikiPane;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;
import org.wikipediacleaner.utils.Configuration;


/**
 * A panel containing a Page.
 */
public class PagePanel
  extends JPanel
  implements ActionListener, PropertyChangeListener {

  /**
   * Serialisation.
   */
  private static final long serialVersionUID = 7122541506066535885L;

  /* ========================================================================== */
  /* Constructors                                                               */
  /* ========================================================================== */

  /**
   * @param wikipedia Wikipedia.
   * @param page Page.
   * @param window Window containing the panel.
   */
  protected PagePanel(
      EnumWikipedia wikipedia, Page page, BasicWindow window) {
    super();
    this.wikipedia = wikipedia;
    this.page = page;
    this.window = window;
  }

  /**
   * @param wikipedia Wikipedia.
   * @param page Page.
   * @param window Window containing the panel.
   * @param layout Layout.
   */
  protected PagePanel(
      EnumWikipedia wikipedia, Page page, BasicWindow window,
      LayoutManager layout) {
    super(layout);
    this.wikipedia = wikipedia;
    this.page = page;
    this.window = window;
  }

  /**
   * @param wikipedia Wikipedia.
   * @param page Page.
   * @param window Window containing the panel.
   * @param isDoubleBuffered
   */
  protected PagePanel(
      EnumWikipedia wikipedia, Page page, BasicWindow window,
      boolean isDoubleBuffered) {
    super(isDoubleBuffered);
    this.wikipedia = wikipedia;
    this.page = page;
    this.window = window;
  }

  /**
   * @param wikipedia Wikipedia.
   * @param page Page.
   * @param window Window containing the panel.
   * @param layout Layout.
   * @param isDoubleBuffered
   */
  protected PagePanel(
      EnumWikipedia wikipedia, Page page, BasicWindow window,
      LayoutManager layout, boolean isDoubleBuffered) {
    super(layout, isDoubleBuffered);
    this.wikipedia = wikipedia;
    this.page = page;
    this.window = window;
  }

  /* ========================================================================== */
  /* Wikipedia management                                                       */
  /* ========================================================================== */

  /**
   * Wikipedia.
   */
  private final EnumWikipedia wikipedia;

  /**
   * @return Wikipedia.
   */
  public EnumWikipedia getWikipedia() {
    return wikipedia;
  }

  /* ========================================================================== */
  /* Page management                                                            */
  /* ========================================================================== */

  /**
   * Page 
   */
  private final Page page;

  /**
   * @return Page.
   */
  public Page getPage() {
    return page;
  }

  /* ========================================================================== */
  /* Parent window management                                                   */
  /* ========================================================================== */

  /**
   * Window containing the panel. 
   */
  private final BasicWindow window;

  /**
   * @return Window containing the panel.
   */
  public BasicWindow getWindow() {
    return window;
  }

  /* ========================================================================== */
  /* GUI management                                                             */
  /* ========================================================================== */

  /**
   * Create panel elements.
   */
  public void createGUI() {

    // Initalize layout
    setLayout(new GridBagLayout());
    GridBagConstraints constraints = new GridBagConstraints();
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridheight = 1;
    constraints.gridwidth = 1;
    constraints.gridx = 0;
    constraints.gridy = 0;
    constraints.insets = new Insets(0, 0, 0, 0);
    constraints.ipadx = 0;
    constraints.ipady = 0;
    constraints.weightx = 0;
    constraints.weighty = 0;

    // Create toolbar
    JToolBar toolbar = createToolbar();
    if (toolbar != null) {
      add(toolbar, constraints);
      constraints.gridy++;
    }

    // Create text contents
    createTextContents();
    constraints.fill = GridBagConstraints.BOTH;
    constraints.weightx = 1;
    constraints.weighty = 1;
  }

  /**
   * Update component state.
   */
  public void updateComponentState() {
    // Compute intermediate states
    boolean pageLoaded = (page != null) && (page.getContents() != null);

    // Update text contents state
    if (textContents != null) {
      textContents.setEnabled(pageLoaded);
    }

    // Update navigation buttons state
    if (buttonFirstOccurrence != null) {
      buttonFirstOccurrence.setEnabled(pageLoaded);
    }
    if (buttonPreviousOccurrence != null) {
      buttonPreviousOccurrence.setEnabled(pageLoaded);
    }
    if (buttonNextOccurrence != null) {
      buttonNextOccurrence.setEnabled(pageLoaded);
    }
    if (buttonLastOccurrence != null) {
      buttonLastOccurrence.setEnabled(pageLoaded);
    }
  }

  /* ========================================================================== */
  /* Toolbar management                                                         */
  /* ========================================================================== */

  /**
   * @return Toolbar.
   */
  protected JToolBar createToolbar() {
    JToolBar toolbar = new JToolBar(SwingConstants.HORIZONTAL);
    JButton button = null;

    // Navigation
    if (isNavigationAvailable()) {
      button = createButtonFirstOccurrence();
      toolbar.add(button);
      button = createButtonPreviousOccurrence();
      toolbar.add(button);
      button = createButtonNextOccurrence();
      toolbar.add(button);
      button = createButtonLastOccurrence();
      toolbar.add(button);
      toolbar.addSeparator();
    }

    // Undo / Redo
    if (isUndoAvailable()) {
      button = createButtonUndo();
      toolbar.add(button);
      button = createButtonRedo();
      toolbar.add(button);
      toolbar.addSeparator();
    }

    // Global page
    button = createButtonView();
    toolbar.add(button);
    button = createButtonViewHistory();
    toolbar.add(button);
    toolbar.addSeparator();

    return toolbar;
  }

  /* ========================================================================== */
  /* Text contents management                                                   */
  /* ========================================================================== */

  /**
   * TextPane.
   */
  private MediaWikiPane textContents;

  /**
   * @return TextPane.
   */
  public MediaWikiPane getTextContents() {
    return textContents;
  }

  /**
   * Create the text contents component.
   */
  protected void createTextContents() {
    if (textContents == null) {
      textContents = new MediaWikiPane(wikipedia, page, window);
      textContents.setBackground(Color.WHITE);
      textContents.setEditable(true);
      if (isUndoAvailable()) {
        Configuration config = Configuration.getConfiguration();
        textContents.setUndoButton(buttonUndo);
        textContents.setRedoButton(buttonRedo);
        textContents.setUndoLevels(config.getInt(
            null,
            Configuration.INTEGER_ANALYSIS_UNDO_LVL,
            Configuration.DEFAULT_ANALYSIS_UNDO_LVL));
      }
      textContents.addPropertyChangeListener(MediaWikiPane.PROPERTY_MODIFIED, this);
    }
  }

  /* ========================================================================== */
  /* Navigation management                                                      */
  /* ========================================================================== */

  private JButton buttonFirstOccurrence;
  private JButton buttonPreviousOccurrence;
  private JButton buttonNextOccurrence;
  private JButton buttonLastOccurrence;

  /**
   * @return True navigation (first, previous, next, last occurrence) is available
   */
  protected boolean isNavigationAvailable() {
    return true;
  }

  /**
   * @return Button for First Occurrence.
   */
  protected JButton createButtonFirstOccurrence() {
    if (buttonFirstOccurrence == null) {
      JButton button = Utilities.createJButton(
          "gnome-go-first.png", EnumImageSize.NORMAL,
          GT._("First occurrence (Alt + &F)"), false);
      button.setActionCommand(ACTION_FIRST_OCCURRENCE);
      button.addActionListener(this);
      buttonFirstOccurrence = button;
    }
    return buttonFirstOccurrence;
  }

  /**
   * @return Button for Previous Occurrence.
   */
  protected JButton createButtonPreviousOccurrence() {
    if (buttonPreviousOccurrence == null) {
      JButton button = Utilities.createJButton(
          "gnome-go-previous.png", EnumImageSize.NORMAL,
          GT._("Previous occurrence (Alt + &P)"), false);
      button.setActionCommand(ACTION_PREVIOUS_OCCURRENCE);
      button.addActionListener(this);
      buttonPreviousOccurrence = button;
    }
    return buttonPreviousOccurrence;
  }

  /**
   * @return Button for Next Occurrence.
   */
  protected JButton createButtonNextOccurrence() {
    if (buttonNextOccurrence == null) {
      JButton button = Utilities.createJButton(
          "gnome-go-next.png", EnumImageSize.NORMAL,
          GT._("Next occurrence (Alt + &N)"), false);
      button.setActionCommand(ACTION_NEXT_OCCURRENCE);
      button.addActionListener(this);
      buttonNextOccurrence = button;
    }
    return buttonNextOccurrence;
  }

  /**
   * @return Button for Last Occurrence.
   */
  protected JButton createButtonLastOccurrence() {
    if (buttonLastOccurrence == null) {
      JButton button = Utilities.createJButton(
          "gnome-go-last.png", EnumImageSize.NORMAL,
          GT._("Last occurrence (Alt + &L)"), false);
      button.setActionCommand(ACTION_LAST_OCCURRENCE);
      button.addActionListener(this);
      buttonLastOccurrence = button;
    }
    return buttonLastOccurrence;
  }

  /**
   * Action called when First Occurence button is pressed. 
   */
  private void actionFirstOccurence() {
    if (textContents != null) {
      textContents.selectFirstOccurence();
      textContents.requestFocusInWindow();
    }
  }

  /**
   * Action called when Previous Occurence button is pressed. 
   */
  private void actionPreviousOccurence() {
    if (textContents != null) {
      textContents.selectPreviousOccurence();
      textContents.requestFocusInWindow();
    }
  }

  /**
   * Action called when Next Occurence button is pressed. 
   */
  private void actionNextOccurence() {
    if (textContents != null) {
      textContents.selectNextOccurence();
      textContents.requestFocusInWindow();
    }
  }

  /**
   * Action called when Last Occurence button is pressed. 
   */
  private void actionLastOccurence() {
    if (textContents != null) {
      getTextContents().selectLastOccurence();
      getTextContents().requestFocusInWindow();
    }
  }

  /* ========================================================================== */
  /* Undo / Redo management                                                     */
  /* ========================================================================== */

  private JButton buttonUndo;
  private JButton buttonRedo;

  /**
   * @return True if undo/redo is available.
   */
  protected boolean isUndoAvailable() {
    return true;
  }

  /**
   * @return Button for Undo.
   */
  protected JButton createButtonUndo() {
    if (buttonUndo == null) {
      JButton button = Utilities.createJButton(
          "gnome-edit-undo.png", EnumImageSize.NORMAL,
          GT._("Undo"), false);
      buttonUndo = button;
    }
    return buttonUndo;
  }

  /**
   * @return Button for Redo.
   */
  protected JButton createButtonRedo() {
    if (buttonRedo == null) {
      JButton button = Utilities.createJButton(
          "gnome-edit-redo.png", EnumImageSize.NORMAL,
          GT._("Redo"), false);
      buttonRedo = button;
    }
    return buttonRedo;
  }

  /* ========================================================================== */
  /* Global page management                                                     */
  /* ========================================================================== */

  private JButton buttonView;
  private JButton buttonViewHistory;

  /**
   * @return Button for External Viewer.
   */
  protected JButton createButtonView() {
    if (buttonView == null) {
      JButton button = Utilities.createJButton(
          "gnome-emblem-web.png", EnumImageSize.NORMAL,
          GT._("External Viewer (Alt + &E)"), false);
      button.setActionCommand(ACTION_VIEW);
      button.addActionListener(this);
      buttonView = button;
    }
    return buttonView;
  }

  /**
   * @return Button for History Viewer.
   */
  protected JButton createButtonViewHistory() {
    if (buttonViewHistory == null) {
      JButton button = Utilities.createJButton(
          "gnome-emblem-documents.png", EnumImageSize.NORMAL,
          GT._("History (Alt + &H)"), false);
      button.setActionCommand(ACTION_VIEW_HISTORY);
      button.addActionListener(this);
      buttonViewHistory = button;
    }
    return buttonViewHistory;
  }

  /**
   * Action called when External Viewer button is pressed. 
   */
  private void actionView() {
    if (page != null) {
      Utilities.browseURL(getWikipedia(), page.getTitle(), false);
    }
  }

  /**
   * Action called when History Viewer button is pressed. 
   */
  private void actionViewHistory() {
    if (page != null) {
      Utilities.browseURL(getWikipedia(), page.getTitle(), "history");
    }
  }

  /* ========================================================================== */
  /* ActionListener                                                             */
  /* ========================================================================== */

  // Commands for Navigation
  private final static String ACTION_FIRST_OCCURRENCE    = "FIRST OCCURRENCE";
  private final static String ACTION_PREVIOUS_OCCURRENCE = "PREVIOUS OCCURRENCE";
  private final static String ACTION_NEXT_OCCURRENCE     = "NEXT OCCURRENCE";
  private final static String ACTION_LAST_OCCURRENCE     = "LAST OCCURRENCE";

  // Commands for Page
  private final static String ACTION_VIEW                = "VIEW";
  private final static String ACTION_VIEW_HISTORY        = "VIEW HISTORY";

  /**
   * Invoked when an action occurs.
   */
  public void actionPerformed(ActionEvent e) {

    // Check inputs
    if (e == null) {
      return;
    }
    String command = e.getActionCommand();
    if (command == null) {
      return;
    }

    // Act depending on the command
    if (command.equals(ACTION_FIRST_OCCURRENCE)) {
      actionFirstOccurence();
    } else if (command.equals(ACTION_PREVIOUS_OCCURRENCE)) {
      actionPreviousOccurence();
    } else if (command.equals(ACTION_NEXT_OCCURRENCE)) {
      actionNextOccurence();
    } else if (command.equals(ACTION_LAST_OCCURRENCE)) {
      actionLastOccurence();
    } else if (command.equals(ACTION_VIEW)) {
      actionView();
    } else if (command.equals(ACTION_VIEW_HISTORY)) {
      actionViewHistory();
    }
  }

  /* ========================================================================== */
  /* PropertyChangeListener                                                     */
  /* ========================================================================== */

  /**
   * @param evt A PropertyChangeEvent object describing the event source and the property that has changed.
   * 
   * @see java.beans.PropertyChangeListener#propertyChange(java.beans.PropertyChangeEvent)
   */
   public void propertyChange(PropertyChangeEvent evt) {
    if (evt == null) {
      return;
    }
    if (MediaWikiPane.PROPERTY_MODIFIED.equals(evt.getPropertyName())) {
      updateComponentState();
    }
  }
}
