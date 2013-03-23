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

import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;


/**
 * Options Window of WikipediaCleaner. 
 */
public class OptionsWindow
  extends BasicWindow {

  public final static Integer WINDOW_VERSION = Integer.valueOf(4);

  public final List<OptionsPanel> panels = new ArrayList<OptionsPanel>();

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
    return GT._("Options");
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
    pane.addTab(GT._("General"), generalOptions);
    panels.add(generalOptions);
    LimitOptionsPanel limitOptions = new LimitOptionsPanel();
    pane.addTab(GT._("Limits"), limitOptions);
    panels.add(limitOptions);
    AnalysisOptionsPanel analysisOptions = new AnalysisOptionsPanel();
    pane.addTab(GT._("Full analysis"), analysisOptions);
    panels.add(analysisOptions);
    SortingOptionsPanel sortingOptions = new SortingOptionsPanel();
    pane.addTab(GT._("Sorting"), sortingOptions);
    panels.add(sortingOptions);
    FormattingOptionsPanel formattingOptions = new FormattingOptionsPanel();
    pane.addTab(GT._("Formatting"), formattingOptions);
    panels.add(formattingOptions);
    DebugOptionsPanel debugOptions = new DebugOptionsPanel();
    pane.addTab(GT._("Debug"), debugOptions);
    panels.add(debugOptions);
    TranslationOptionsPanel translationOptions = new TranslationOptionsPanel();
    pane.addTab(GT._("Translation"), translationOptions);
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
    buttonApply = Utilities.createJButton(GT._("&Apply"));
    buttonApply.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionApply"));
    panel.add(buttonApply);

    // Validate button
    buttonValidate = Utilities.createJButton(GT._("&Validate"));
    buttonValidate.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionValidate"));
    panel.add(buttonValidate);

    // Cancel button
    buttonCancel = Utilities.createJButton(GT._("&Cancel"));
    buttonCancel.addActionListener(EventHandler.create(
        ActionListener.class, this, "dispose"));
    panel.add(buttonCancel);

    // Restore defaults button
    buttonDefault = Utilities.createJButton(GT._("&Restore defaults"));
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
