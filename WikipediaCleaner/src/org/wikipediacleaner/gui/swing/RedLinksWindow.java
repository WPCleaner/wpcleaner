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

package org.wikipediacleaner.gui.swing;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.util.Vector;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.WindowConstants;
import javax.swing.border.TitledBorder;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.gui.swing.basic.DefaultBasicWindowListener;
import org.wikipediacleaner.gui.swing.basic.DefaultBasicWorkerListener;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.component.MediaWikiPane;
import org.wikipediacleaner.gui.swing.component.PageListModel;
import org.wikipediacleaner.gui.swing.worker.RedLinksAnalysisWorker;
import org.wikipediacleaner.i18n.GT;


/**
 * Red links analysis window.
 */
public class RedLinksWindow extends BasicWindow implements ActionListener, ItemListener {

  private final static String ACTION_CLOSE    = "CLOSE";
  private final static String ACTION_RELOAD   = "RELOAD";
  private final static String ACTION_REPLACE  = "REPLACE";
  private final static String ACTION_SEARCH   = "SEARCH";
  private final static String ACTION_TYPE     = "SEARCH TYPE";

  Page           page;
  MediaWikiPane  textPane;

  private JButton buttonClose;
  private JButton buttonReload;
  private JButton buttonReplace;

  private JCheckBox chkOnlyPage;
  JList listPages;
  PageListModel modelPages;

  private JRadioButton buttonBegin;
  private JComboBox comboBegin;
  private JRadioButton buttonContain;
  private JComboBox comboContain;
  private JRadioButton buttonEnd;
  private JComboBox comboEnd;
  private JButton buttonSearch;

  JList listLinks;
  PageListModel modelLinks;

  boolean pageLoaded = false;

  /**
   * Create and display a RedLinksWindow.
   * 
   * @param page Page name.
   * @param wikipedia Wikipedia.
   */
  public static void createRedLinksWindow(
      final Page page,
      final MediaWikiPane textPane,
      final EnumWikipedia wikipedia) {
    if ((page == null) || (wikipedia == null)) {
      return;
    }
    createWindow(
        "RedLinksWindow",
        wikipedia,
        WindowConstants.DISPOSE_ON_CLOSE,
        RedLinksWindow.class,
        new DefaultBasicWindowListener() {
          @Override
          public void initializeWindow(BasicWindow window) {
            if (window instanceof RedLinksWindow) {
              RedLinksWindow analysis = (RedLinksWindow) window;
              analysis.page = page;
              analysis.textPane = textPane;
              analysis.modelPages = new PageListModel();
              analysis.modelPages.setShowDisambiguation(true);
              analysis.modelPages.setShowMissing(true);
              analysis.modelPages.setShowOther(true);
              analysis.modelPages.setShowRedirect(true);
              analysis.modelLinks = new PageListModel();
              analysis.modelLinks.setShowDisambiguation(true);
              analysis.modelLinks.setShowMissing(true);
              analysis.modelLinks.setShowOther(true);
              analysis.modelLinks.setShowRedirect(true);
            }
          }
          @Override
          public void displayWindow(BasicWindow window) {
            if (window instanceof RedLinksWindow) {
              RedLinksWindow analysis = (RedLinksWindow) window;
              analysis.actionReload();
            }
          }
        });
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.basic.BasicWindow#getTitle()
   */
  @Override
  public String getTitle() {
    return GT._("Red links Analysis - {0}", page.getTitle());
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
    constraints.weightx = 0;
    constraints.weighty = 0;

    // Lists
    constraints.fill = GridBagConstraints.BOTH;
    constraints.weightx = 1;
    constraints.weighty = 1;
    panel.add(createPageComponents(), constraints);
    constraints.gridx++;
    panel.add(createLinksComponents(), constraints);
    constraints.gridy = 0;
    constraints.gridy++;

    // Commands
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridwidth = 2;
    constraints.gridx = 0;
    constraints.weightx = 1;
    constraints.weighty = 0;
    panel.add(createCommandComponents(), constraints);
    constraints.gridy++;

    updateComponentState();
    return panel;
  }

  /**
   * Update component state.
   */
  @Override
  protected void updateComponentState() {
    chkOnlyPage.setEnabled(textPane != null);
    listPages.setEnabled(!chkOnlyPage.isSelected());
    comboBegin.setEnabled(buttonBegin.isSelected());
    comboContain.setEnabled(buttonContain.isSelected());
    comboEnd.setEnabled(buttonEnd.isSelected());
  }

  /**
   * @return Command components.
   */
  private Component createCommandComponents() {
    JPanel panel = new JPanel(new FlowLayout(FlowLayout.CENTER, 5, 0));

    // Replace button
    buttonReplace = Utilities.createJButton(GT._("Re&place"));
    buttonReplace.setActionCommand(ACTION_REPLACE);
    buttonReplace.addActionListener(this);
    panel.add(buttonReplace);

    // Close button
    buttonClose = Utilities.createJButton(GT._("&Close"));
    buttonClose.setActionCommand(ACTION_CLOSE);
    buttonClose.addActionListener(this);
    panel.add(buttonClose);

    return panel;
  }

  /**
   * @return Page components.
   */
  private Component createPageComponents() {
    JPanel panel = new JPanel(new GridBagLayout());
    panel.setBorder(new TitledBorder(GT._("Pages concerned")));

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

    // Checkbox for applying changes
    chkOnlyPage = Utilities.createJCheckBox(
        GT._("&Apply only to {0}", page.getTitle()),
        false);
    chkOnlyPage.addItemListener(this);
    panel.add(chkOnlyPage, constraints);
    constraints.gridy++;

    // Pages
    constraints.fill = GridBagConstraints.BOTH;
    constraints.weighty = 1;
    listPages = new JList(modelPages);
    listPages.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
    listPages.getSelectionModel().setSelectionInterval(0, modelPages.getSize());
    JScrollPane scrollPages = new JScrollPane(listPages);
    scrollPages.setMinimumSize(new Dimension(100, 100));
    scrollPages.setPreferredSize(new Dimension(200, 500));
    scrollPages.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
    panel.add(scrollPages, constraints);
    constraints.gridy++;

    // Reload button
    buttonReload = Utilities.createJButton(GT._("&Reload page list"));
    buttonReload.setActionCommand(ACTION_RELOAD);
    buttonReload.addActionListener(this);
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.weighty = 0;
    panel.add(buttonReload, constraints);
    constraints.gridy++;

    return panel;
  }

  /**
   * @return Links components.
   */
  private Component createLinksComponents() {
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

    // Search
    panel.add(createSearchComponents(), constraints);
    constraints.gridy++;

    // Pages
    listLinks = new JList(modelLinks);
    JScrollPane scrollLinks = new JScrollPane(listLinks);
    scrollLinks.setMinimumSize(new Dimension(100, 100));
    scrollLinks.setPreferredSize(new Dimension(200, 500));
    scrollLinks.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
    JPanel panelLinks = new JPanel(new BorderLayout());
    panelLinks.setBorder(new TitledBorder(GT._("Possible pages")));
    panelLinks.add(scrollLinks);
    constraints.fill = GridBagConstraints.BOTH;
    constraints.weighty = 1;
    panel.add(panelLinks, constraints);
    constraints.gridy++;

    return panel;
  }

  /**
   * @return Search components.
   */
  private Component createSearchComponents() {
    JPanel panel = new JPanel(new GridBagLayout());
    panel.setBorder(new TitledBorder(GT._("Search")));

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

    ButtonGroup grpSearch = new ButtonGroup();

    // Begin with
    Vector<String> values = new Vector<String>();
    final String separators = " ,(";
    String title = page.getTitle();
    for (int i = 1; i < title.length(); i++) {
      char char1 = title.charAt(i - 1);
      char char2 = title.charAt(i);
      if ((separators.indexOf(char1) == -1) && (separators.indexOf(char2) != -1)) {
        values.add(title.substring(0, i));
      }
    }
    if (separators.indexOf(title.charAt(title.length() - 1)) == -1) {
      values.add(title);
    }
    buttonBegin = Utilities.createJRadioButton(GT._("&Begins with"), true);
    buttonBegin.setActionCommand(ACTION_TYPE);
    buttonBegin.addActionListener(this);
    grpSearch.add(buttonBegin);
    constraints.gridx = 0;
    constraints.weightx = 0;
    panel.add(buttonBegin, constraints);
    constraints.gridx++;
    comboBegin = new JComboBox(values);
    comboBegin.setEditable(true);
    panel.add(comboBegin, constraints);
    constraints.gridy++;

    // Contain
    buttonContain = Utilities.createJRadioButton(GT._("&Contains"), true);
    buttonContain.setActionCommand(ACTION_TYPE);
    buttonContain.addActionListener(this);
    buttonContain.setEnabled(false);
    grpSearch.add(buttonContain);
    constraints.gridx = 0;
    constraints.weightx = 0;
    panel.add(buttonContain, constraints);
    constraints.gridx++;
    comboContain = new JComboBox();
    comboContain.setEditable(true);
    panel.add(comboContain, constraints);
    constraints.gridy++;

    // End with
    buttonEnd = Utilities.createJRadioButton(GT._("&Ends with"), true);
    buttonEnd.setActionCommand(ACTION_TYPE);
    buttonEnd.addActionListener(this);
    buttonEnd.setEnabled(false);
    grpSearch.add(buttonEnd);
    constraints.gridx = 0;
    constraints.weightx = 0;
    panel.add(buttonEnd, constraints);
    constraints.gridx++;
    comboEnd = new JComboBox();
    comboEnd.setEditable(true);
    panel.add(comboEnd, constraints);
    constraints.gridy++;

    // Search
    buttonSearch = Utilities.createJButton(GT._("&Search for possible pages"));
    buttonSearch.setActionCommand(ACTION_SEARCH);
    buttonSearch.addActionListener(this);
    constraints.gridwidth = 2;
    constraints.gridx = 0;
    panel.add(buttonSearch, constraints);
    constraints.gridy++;

    return panel;
  }

  /**
   * Invoked when an action occurs.
   * 
   * @param e Event.
   */
  @Override
  public void actionPerformed(ActionEvent e) {
    if (e == null) {
      return;
    }

    if (ACTION_RELOAD.equals(e.getActionCommand())) {
      actionReload();
    } else if (ACTION_REPLACE.equals(e.getActionCommand())) {
      actionReplace();
    } else if (ACTION_CLOSE.equals(e.getActionCommand())) {
      actionClose();
    } else if (ACTION_TYPE.equals(e.getActionCommand())) {
      updateComponentState();
    }
  }

  /* (non-Javadoc)
   * @see java.awt.event.ItemListener#itemStateChanged(java.awt.event.ItemEvent)
   */
  public void itemStateChanged(ItemEvent e) {
    if (e == null) {
      return;
    }

    if (e.getItem() == chkOnlyPage) {
      updateComponentState();
    }
  }

  /**
   * Action called when Reload button is pressed. 
   */
  void actionReload() {
    RedLinksAnalysisWorker reloadWorker = new RedLinksAnalysisWorker(getWikipedia(), this, page);
    reloadWorker.setListener(new DefaultBasicWorkerListener () {
      @Override
      public void afterFinished(
          @SuppressWarnings("unused") BasicWorker worker,
          @SuppressWarnings("unused") boolean ok) {
        modelPages.clear();
        for (Page p : page.getBackLinksWithRedirects()) {
          modelPages.addElement(p);
        }
      }
    });
    reloadWorker.start();
  }

  /**
   * Action called when Replace button is pressed. 
   */
  private void actionReplace() {
    //TODO
  }

  /**
   * Action called when Close button is pressed.
   */
  private void actionClose() {
    dispose();
  }
}
