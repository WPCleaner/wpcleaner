/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.options;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.event.ActionListener;
import java.beans.EventHandler;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.WindowConstants;

import org.wikipediacleaner.gui.swing.action.ActionDispose;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueShortcut;


/**
 * Options Window of WikipediaCleaner. 
 */
public class OptionsWindow
  extends BasicWindow {

  public final static Integer WINDOW_VERSION = Integer.valueOf(5);

  public final List<OptionsPanel> panels = new ArrayList<>();

  private JButton buttonApply;
  private JButton buttonCancel;
  private JButton buttonDefault;
  private JButton buttonValidate;

  /**
   * Create and display an OptionsWindow.
   */
  public static void createOptionsWindow() {
    createWindow(
        "OptionsWindow",
        null,
        WindowConstants.DISPOSE_ON_CLOSE,
        OptionsWindow.class,
        null);
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.basic.BasicWindow#getTitle()
   */
  @Override
  public String getTitle() {
    return GT._T("Options");
  }

  /**
   * @return Window components.
   */
  @Override
  protected Component createComponents() {
    JPanel panel = new JPanel();
    panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
    panel.add(createOptionsComponents());
    panel.add(createCommandComponents());
    return panel;
  }

  /**
   * @return Login components.
   */
  private Component createOptionsComponents() {
    JPanel panel = new JPanel(new BorderLayout());

    // Create tabbed pane
    JTabbedPane pane = new JTabbedPane();
    GeneralOptionsPanel generalOptions = new GeneralOptionsPanel();
    pane.addTab(GT._T("General"), generalOptions);
    panels.add(generalOptions);
    LimitOptionsPanel limitOptions = new LimitOptionsPanel();
    pane.addTab(GT._T("Limits"), limitOptions);
    panels.add(limitOptions);
    AnalysisOptionsPanel analysisOptions = new AnalysisOptionsPanel();
    pane.addTab(GT._T("Full analysis"), analysisOptions);
    panels.add(analysisOptions);
    SortingOptionsPanel sortingOptions = new SortingOptionsPanel();
    pane.addTab(GT._T("Sorting"), sortingOptions);
    panels.add(sortingOptions);
    FormattingOptionsPanel formattingOptions = new FormattingOptionsPanel();
    pane.addTab(GT._T("Formatting"), formattingOptions);
    panels.add(formattingOptions);
    ShortcutOptionsPanel shortcutOptions = new ShortcutOptionsPanel();
    pane.addTab(GT._T("Shortcuts"), shortcutOptions);
    panels.add(shortcutOptions);
    DebugOptionsPanel debugOptions = new DebugOptionsPanel();
    pane.addTab(GT._T("Debug"), debugOptions);
    panels.add(debugOptions);
    TranslationOptionsPanel translationOptions = new TranslationOptionsPanel();
    pane.addTab(GT._T("Translation"), translationOptions);
    panels.add(translationOptions);
    panel.add(pane);

    return panel;
  }

  /**
   * @return Login components.
   */
  private Component createCommandComponents() {
    JPanel panel = new JPanel(new FlowLayout(FlowLayout.CENTER));

    // Apply button
    buttonApply = Utilities.createJButton(
        GT._T("Apply"),
        ConfigurationValueShortcut.APPLY);
    buttonApply.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionApply"));
    panel.add(buttonApply);

    // Validate button
    buttonValidate = Utilities.createJButton(
        GT._T("Validate"),
        ConfigurationValueShortcut.VALIDATE);
    buttonValidate.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionValidate"));
    panel.add(buttonValidate);

    // Cancel button
    buttonCancel = ActionDispose.createButton(getParentComponent(), true, true);
    panel.add(buttonCancel);

    // Restore defaults button
    buttonDefault = Utilities.createJButton(
        GT._T("Restore defaults"),
        ConfigurationValueShortcut.RESTORE_DEFAULTS);
    buttonDefault.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionDefault"));
    panel.add(buttonDefault);

    return panel;
  }

  /**
   * Action called when Apply button is pressed.
   */
  public void actionApply() {
    for (OptionsPanel panel : panels) {
      panel.apply();
    }
    Configuration config = Configuration.getConfiguration();
    config.updateConfiguration();
  }

  /**
   * Action called when Default button is pressed.
   */
  public void actionDefault() {
    for (OptionsPanel panel : panels) {
      panel.defaultValues();
    }
  }

  /**
   * Action called when Validate button is pressed.
   */
  public void actionValidate() {
    actionApply();
    dispose();
  }
}
