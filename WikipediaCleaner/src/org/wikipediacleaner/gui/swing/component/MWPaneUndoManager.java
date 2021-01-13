/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.component;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.LinkedList;

import javax.swing.JButton;


/**
 * Manage Undo/Redo operations on MWPane.
 */
public class MWPaneUndoManager implements ActionListener {

  /**
   * MWPane.
   */
  private final MWPane textPane;

  MWPaneUndoManager(MWPane textPane) {
    this.textPane = textPane;
    this.undoLevels = 0;
    this.undoTexts = new LinkedList<>();
    this.redoTexts = new LinkedList<>();
  }

  /* ======================================================================= */
  /* UNDO levels                                                             */
  /* ======================================================================= */

  /**
   * Maximum number of Undo levels.
   */
  private int undoLevels;

  /**
   * @param levels Number of undo levels.
   */
  public void setUndoLevels(int levels) {
    this.undoLevels = levels;
  }

  /* ======================================================================= */
  /* Text management                                                         */
  /* ======================================================================= */

  /**
   * Previous texts for Undo.
   */
  private LinkedList<String> undoTexts;

  /**
   * Previous texts for Redo.
   */
  private LinkedList<String> redoTexts;

  /**
   * Clear Undo/Redo texts.
   */
  void clear() {
    undoTexts.clear();
    redoTexts.clear();
  }

  /* ======================================================================= */
  /* Action management                                                       */
  /* ======================================================================= */

  /**
   * Undo action.
   */
  private final static String ACTION_UNDO = "UNDO";

  /**
   * Redo action.
   */
  private final static String ACTION_REDO = "REDO";

  /**
   * Memorize current text for undo / redo.
   */
  void validateCurrentText() {
    if (undoLevels <= 0) {
      return;
    }

    // Check if memorizing text is useful
    String currentText = textPane.getText();
    if (!undoTexts.isEmpty() && currentText.equals(undoTexts.getLast())) {
      return;
    }

    // Adding text
    undoTexts.addLast(currentText);
    while (undoTexts.size() > undoLevels) {
      undoTexts.removeFirst();
    }
    redoTexts.clear();
    updateUndoButtons();
  }

  /**
   * Undo last change.
   */
  private void undo() {
    if (undoTexts.isEmpty()) {
      return;
    }
    String newText = undoTexts.getLast();
    String oldText = textPane.getText();
    if (oldText.equals(newText)) {
      if (undoTexts.size() < 1) {
        return;
      }
      undoTexts.removeLast();
      if (undoTexts.isEmpty()) {
        return;
      }
      newText = undoTexts.getLast();
    }
    undoTexts.removeLast();
    redoTexts.addLast(oldText);
    textPane.changeText(newText);
    updateUndoButtons();
  }

  /**
   * Redo last change.
   */
  private void redo() {
    if (redoTexts.isEmpty()) {
      return;
    }
    String newText = redoTexts.getLast();
    String oldText = textPane.getText();
    if (oldText.equals(newText)) {
      if (redoTexts.size() < 1) {
        return;
      }
      redoTexts.removeLast();
      newText = redoTexts.getLast();
    }
    redoTexts.removeLast();
    undoTexts.addLast(oldText);
    textPane.changeText(newText);
    updateUndoButtons();
  }

  /**
   * @param e Event.
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed(ActionEvent e) {
    if ((e == null) || (e.getActionCommand() == null)) {
      return;
    }
    if (e.getActionCommand().equals(ACTION_UNDO)) {
      undo();
    } else if (e.getActionCommand().equals(ACTION_REDO)) {
      redo();
    }
  }

  /* ======================================================================= */
  /* Buttons management                                                      */
  /* ======================================================================= */

  /**
   * Undo button.
   */
  private JButton undoButton;

  /**
   * Redo button.
   */
  private JButton redoButton;

  /**
   * Update status of Undo / Redo buttons.
   */
  void updateUndoButtons() {
    if (undoButton != null) {
      undoButton.setEnabled(!undoTexts.isEmpty() && textPane.isModified());
    }
    if (redoButton != null) {
      redoButton.setEnabled(!redoTexts.isEmpty());
    }
  }

  /**
   * @param undo Undo button.
   */
  public void setUndoButton(JButton undo) {
    if (undoButton != null) {
      undoButton.removeActionListener(this);
    }
    undoButton = undo;
    if (undoButton != null) {
      undoButton.setActionCommand(ACTION_UNDO);
      undoButton.addActionListener(this);
    }
    updateUndoButtons();
  }

  /**
   * @param redo Redo button.
   */
  public void setRedoButton(JButton redo) {
    if (redoButton != null) {
      redoButton.removeActionListener(this);
    }
    redoButton = redo;
    if (redoButton != null) {
      redoButton.setActionCommand(ACTION_REDO);
      redoButton.addActionListener(this);
    }
    updateUndoButtons();
  }
}
