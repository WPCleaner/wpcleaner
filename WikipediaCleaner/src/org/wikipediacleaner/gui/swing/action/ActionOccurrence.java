/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.action;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JToolBar;

import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.gui.swing.component.MWPaneSelectionManager;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;
import org.wikipediacleaner.utils.ConfigurationValueShortcut;


/**
 * Manage actions for finding occurrences in a MWPane.
 */
public class ActionOccurrence implements ActionListener {

  /**
   * Enumeration for possible occurrences
   */
  public enum Occurrence {
    FIRST,
    LAST,
    NEXT,
    PREVIOUS
  }

  /**
   * @param occurrence Kind of occurrence.
   * @param showIcon True if the button should use an icon.
   * @param useShortcut True if shortcut should be used.
   * @return Button.
   */
  private static JButton createInternalButton(
      Occurrence occurrence,
      boolean showIcon, boolean useShortcut) {
    ConfigurationValueShortcut shortcut = ConfigurationValueShortcut.OCCURRENCE_NEXT;
    String iconName = "gnome-go-next.png";
    String label = GT._T("Next occurrence");
    switch (occurrence) {
    case FIRST:
      shortcut = ConfigurationValueShortcut.OCCURRENCE_FIRST;
      iconName = "gnome-go-first.png";
      label = GT._T("First occurrence");
      break;
    case LAST:
      shortcut = ConfigurationValueShortcut.OCCURRENCE_LAST;
      iconName = "gnome-go-last.png";
      label = GT._T("Last occurrence");
      break;
    case NEXT:
      shortcut = ConfigurationValueShortcut.OCCURRENCE_NEXT;
      iconName = "gnome-go-next.png";
      label = GT._T("Next occurrence");
      break;
    case PREVIOUS:
      shortcut = ConfigurationValueShortcut.OCCURRENCE_PREVIOUS;
      iconName = "gnome-go-previous.png";
      label = GT._T("Previous occurrence");
      break;
    }
    return Utilities.createJButton(
        showIcon ? iconName : null,
        EnumImageSize.NORMAL,
        label, !showIcon,
        useShortcut ? shortcut : null);
  }

  /**
   * Create a button for adding selected pages to the Watch list.
   * 
   * @param pane MWPane.
   * @param occurrence Kind of occurrence.
   * @param showIcon True if the button should use an icon.
   * @param useShortcut True if shortcut should be used.
   * @return Button.
   */
  public static JButton createButton(
      MWPane pane, Occurrence occurrence,
      boolean showIcon, boolean useShortcut) {
    JButton button = createInternalButton(occurrence, showIcon, useShortcut);
    button.addActionListener(new ActionOccurrence(pane, occurrence));
    return button;
  }

  /**
   * Add a button for analyzing a page.
   * 
   * @param toolbar Tool bar.
   * @param pane MWPane.
   * @param occurrence Kind of occurrence.
   * @param showIcon True if the button should use an icon.
   * @param useShortcut True if shortcut should be used.
   * @return Button.
   */
  public static JButton addButton(
      JToolBar toolbar,
      MWPane pane, Occurrence occurrence,
      boolean showIcon, boolean useShortcut) {
    JButton button = createButton(pane, occurrence, showIcon, useShortcut);
    if ((button != null) && (toolbar != null)) {
      toolbar.add(button);
    }
    return button;
  }

  /**
   * MWPane on which the action should be applied.
   */
  private final MWPane pane;

  /**
   * Kind of occurrence we should find.
   */
  private final Occurrence occurrence;

  /**
   * @param parent Parent component.
   * @param wiki Wiki.
   * @param list Selected pages should be added to the watch list.
   */
  private ActionOccurrence(MWPane pane, Occurrence occurrence) {
    this.pane = pane;
    this.occurrence = occurrence;
  }

  /**
   * Find occurrence.
   * 
   * @param e Event triggering this call.
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed(ActionEvent e) {
    if ((e == null) || (pane == null)) {
      return;
    }

    // Find occurrence
    MWPaneSelectionManager selection = pane.getSelectionManager();
    switch (occurrence) {
    case FIRST:
      selection.selectFirstOccurrence();
      break;
    case LAST:
      selection.selectLastOccurrence();
      break;
    case NEXT:
      selection.selectNextOccurrence();
      break;
    case PREVIOUS:
      selection.selectPreviousOccurrence();
      break;
    }
    pane.requestFocusInWindow();
  }
}
