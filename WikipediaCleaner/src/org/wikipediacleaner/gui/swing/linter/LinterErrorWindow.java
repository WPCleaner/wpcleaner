/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.linter;

import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.WindowConstants;
import javax.swing.text.JTextComponent;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.linter.LinterError;
import org.wikipediacleaner.gui.swing.action.ActionDispose;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.DefaultBasicWindowListener;
import org.wikipediacleaner.i18n.GT;


/**
 * Linter errors. 
 */
public class LinterErrorWindow extends BasicWindow {

  /** Linter errors to be displayed */
  private List<LinterError> errors;

  /** Close button */
  private JButton buttonClose;

  /** Text pane */
  private JTextComponent textPane;

  /**
   * Create and display an LinterErrorWindow.
   * 
   * @param wiki Wiki.
   * @param errors List of Linter errors.
   * @param textPane Text pane.
   */
  public static void createLinterErrorWindow(
      EnumWikipedia wiki,
      final List<LinterError> errors,
      final JTextComponent textPane) {
    createWindow(
        "LinterWindow", wiki,
        WindowConstants.DISPOSE_ON_CLOSE,
        LinterErrorWindow.class,
        new DefaultBasicWindowListener() {
          
          @Override
          public void initializeWindow(BasicWindow window) {
            LinterErrorWindow.initializeWindow(window, errors, textPane);
          }
        });
  }

  /**
   * Initialize window.
   * 
   * @param window Window.
   * @param errors List of Linter errors.
   * @param textPane Text pane.
   */
  static void initializeWindow(
      BasicWindow window,
      List<LinterError> errors,
      JTextComponent textPane) {
    if (window instanceof LinterErrorWindow) {
      LinterErrorWindow linterError = (LinterErrorWindow) window;
      linterError.setErrors(errors);
      linterError.setTextPane(textPane);
    }
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.basic.BasicWindow#getTitle()
   */
  @Override
  public String getTitle() {
    return GT._T("Errors"); 
  }

  /**
   * @param errors List of linter errors.
   */
  protected void setErrors(List<LinterError> errors) {
    this.errors = errors;
  }

  /**
   * @param textPane Text pane.
   */
  protected void setTextPane(JTextComponent textPane) {
    this.textPane = textPane;
  }

  /**
   * @return Window components.
   */
  @Override
  protected Component createComponents() {
    JPanel panel = new JPanel(new GridBagLayout());

    // Initialize constraints
    GridBagConstraints constraints = new GridBagConstraints();
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridheight = 1;
    constraints.gridwidth = 1;
    constraints.gridx = 0;
    constraints.gridy = 0;
    constraints.insets = new Insets(2, 2, 2, 2);
    constraints.ipadx = 0;
    constraints.ipady = 0;
    constraints.weightx = 1;
    constraints.weighty = 0;

    // About
    constraints.fill = GridBagConstraints.BOTH;
    constraints.weighty = 1;
    panel.add(new LinterErrorPanel(getWiki(), errors, textPane), constraints);
    constraints.gridy++;

    // Commands
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.weighty = 0;
    panel.add(createCommandComponents(), constraints);
    constraints.gridy++;

    return panel;
  }

  /**
   * @return Command components.
   */
  private Component createCommandComponents() {
    JPanel panel = new JPanel(new FlowLayout(FlowLayout.CENTER));
    //panel.setBorder(BorderFactory.createEtchedBorder());

    // Close button
    buttonClose = ActionDispose.createButton(getParentComponent(), true, false);
    panel.add(buttonClose);

    return panel;
  }
}
