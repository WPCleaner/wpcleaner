/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.beans.EventHandler;
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
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.gui.swing.component.PageListModel;
import org.wikipediacleaner.gui.swing.worker.RedLinksAnalysisWorker;
import org.wikipediacleaner.i18n.GT;


/**
 * Red links analysis window.
 */
public class RedLinksWindow extends BasicWindow implements ActionListener, ItemListener {

  Page    page;
  MWPane  textPane;

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
      final MWPane textPane,
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
  public void updateComponentState() {
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
    buttonReplace.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionReplace"));
    panel.add(buttonReplace);

    // Close button
    buttonClose = Utilities.createJButton(GT._("&Close"));
    buttonClose.addActionListener(EventHandler.create(
        ActionListener.class, this, "dispose"));
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
    buttonReload.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionReload"));
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
    buttonBegin.addActionListener(EventHandler.create(
        ActionListener.class, this, "updateComponentState"));
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
    buttonContain.addActionListener(EventHandler.create(
        ActionListener.class, this, "updateComponentState"));
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
    buttonEnd.addActionListener(EventHandler.create(
        ActionListener.class, this, "updateComponentState"));
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
    constraints.gridwidth = 2;
    constraints.gridx = 0;
    panel.add(buttonSearch, constraints);
    constraints.gridy++;

    return panel;
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
  public void actionReload() {
    RedLinksAnalysisWorker reloadWorker = new RedLinksAnalysisWorker(getWikipedia(), this, page);
    reloadWorker.setListener(new DefaultBasicWorkerListener () {
      @Override
      public void afterFinished(
          @SuppressWarnings("unused") BasicWorker worker,
          @SuppressWarnings("unused") boolean ok) {
        modelPages.setElements(page.getBackLinksWithRedirects());
      }
    });
    reloadWorker.start();
  }

  /**
   * Action called when Replace button is pressed. 
   */
  public void actionReplace() {
    //TODO
  }
}
