/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionListener;
import java.awt.font.TextAttribute;
import java.beans.EventHandler;
import java.io.File;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.JTextPane;
import javax.swing.JToolBar;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;
import javax.swing.WindowConstants;
import javax.swing.filechooser.FileFilter;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.table.TableColumnModel;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmComparator;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithms;
import org.wikipediacleaner.api.constants.CWConfigurationError;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.AutomaticFixing;
import org.wikipediacleaner.api.data.AutomaticFixingList;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageComparator;
import org.wikipediacleaner.gui.swing.action.ActionDisambiguationAnalysis;
import org.wikipediacleaner.gui.swing.action.ActionExternalViewer;
import org.wikipediacleaner.gui.swing.action.ActionFullAnalysis;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.gui.swing.basic.DefaultBasicWindowListener;
import org.wikipediacleaner.gui.swing.basic.DefaultBasicWorkerListener;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.component.PageListCellRenderer;
import org.wikipediacleaner.gui.swing.component.PageListModel;
import org.wikipediacleaner.gui.swing.worker.AutomaticFixingWorker;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueString;


/**
 * Automatic fixing window.
 */
public class AutomaticFixingWindow extends OnePageWindow {

  //public final static Integer WINDOW_VERSION = Integer.valueOf(2);

  private JTable tableAutomaticFixing;
  private AutomaticFixingTableModel modelAutomaticFixing;
  private JButton buttonAdd;
  private JButton buttonModify;
  private JButton buttonRemove;
  private JButton buttonClear;
  private JButton buttonRun;
  private JButton buttonSave;
  private JButton buttonTest;
  private JButton buttonLoadList;
  private JButton buttonSaveList;
  private JButton buttonAutomaticCWAlgorithms;
  private JButton buttonForceCWAlgorithms;
  private JCheckBox chkWaitAfterEdit;
  private JTextPane paneOriginal;
  private JTextPane paneResult;

  JList<Page> listPages;
  PageListModel modelPages;
  private PageListCellRenderer listCellRenderer;
  private List<CheckErrorAlgorithm> automaticCWAlgorithms;
  private List<CheckErrorAlgorithm> forceCWAlgorithms;

  Collection<Page> pages;

  /**
   * Create and display an AutomaticFixingWindow.
   * 
   * @param pages List of pages for automatic fixing.
   * @param referencePage Reference page.
   * @param wikipedia Wikipedia.
   */
  public static void createAutomaticFixingWindow(
      final Collection<Page> pages,
      final Page referencePage,
      final EnumWikipedia wikipedia) {
    createWindow(
        "AutomaticFixingWindow",
        wikipedia,
        WindowConstants.DISPOSE_ON_CLOSE,
        AutomaticFixingWindow.class,
        new DefaultBasicWindowListener() {
          @Override
          public void initializeWindow(BasicWindow window) {
            if (window instanceof AutomaticFixingWindow) {
              AutomaticFixingWindow fixing = (AutomaticFixingWindow) window;
              fixing.setPage(referencePage);
              fixing.pages = pages;
              fixing.modelPages = new PageListModel();
              fixing.modelPages.setComparator(PageComparator.getNamespaceFirstComparator());
              fixing.modelPages.setShowDisambiguation(true);
              fixing.modelPages.setShowOther(true);
              fixing.modelPages.setShowRedirect(true);
            }
          }
          @Override
          public void displayWindow(BasicWindow window) {
            if (window instanceof AutomaticFixingWindow) {
              AutomaticFixingWindow fixing = (AutomaticFixingWindow) window;
              fixing.actionReload();
            }
          }
        });
  }

  /**
   * @return Window title.
   * @see org.wikipediacleaner.gui.swing.basic.BasicWindow#getTitle()
   */
  @Override
  public String getTitle() {
    if (getPageName() != null) {
      return GT._("Automatic fixing") + " - " + getPageName();
    }
    return GT._("Automatic fixing");
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

    // Contents
    constraints.fill = GridBagConstraints.BOTH;
    constraints.gridwidth = 1;
    constraints.gridx = 0;
    constraints.weightx = 1;
    constraints.weighty = 1;
    JSplitPane split = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
    split.setLeftComponent(createLinksComponents());
    split.setRightComponent(createAutomaticFixingComponents());
    split.setPreferredSize(new Dimension(1000, 700));
    split.setMinimumSize(new Dimension(200, 200));
    split.setResizeWeight(0.0);
    split.setDividerLocation(200 + split.getInsets().left);
    panel.add(split, constraints);
    constraints.gridy++;

    updateComponentState();
    return panel;
  }

  /**
   * @return Automatic fixing components.
   */
  private Component createAutomaticFixingComponents() {
    JPanel panel = new JPanel(new GridBagLayout());

    // Initialize constraints
    GridBagConstraints constraints = new GridBagConstraints();
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridheight = 1;
    constraints.gridwidth = 1;
    constraints.gridx = 0;
    constraints.gridy = 0;
    constraints.insets = new Insets(1, 1, 1, 1);
    constraints.ipadx = 0;
    constraints.ipady = 0;
    constraints.weightx = 1;
    constraints.weighty = 0;

    // Warning
    String txtWarning =
      GT._("This feature may modify a lot of pages in a short period of time.") + "\n" +
      GT._("On some Wikipedia projects, you may need the bot status for doing this.") + "\n" +
      GT._("Please, check if you need the bot status by reading the rules of Wikipedia.");
    JTextArea lblWarning = new JTextArea(txtWarning);
    lblWarning.setEditable(false);
    lblWarning.setBackground(getParentComponent().getBackground());
    lblWarning.setForeground(Color.RED);
    constraints.fill = GridBagConstraints.BOTH;
    constraints.gridwidth = 2;
    constraints.weighty = 0;
    panel.add(lblWarning, constraints);
    constraints.gridy++;

    // Commands
    JToolBar toolBarButtons = new JToolBar(SwingConstants.HORIZONTAL);
    toolBarButtons.setFloatable(false);
    buttonAdd = Utilities.createJButton(
        "gnome-list-add.png", EnumImageSize.NORMAL,
        GT._("Add"), false, null);
    buttonAdd.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionAddAutomaticFixing"));
    toolBarButtons.add(buttonAdd);
    buttonRemove = Utilities.createJButton(
        "gnome-list-remove.png", EnumImageSize.NORMAL,
        GT._("Remove"), false, null);
    buttonRemove.addActionListener(EventHandler.create(
        ActionListener.class, this, "ActionRmvAutomaticFixing"));
    toolBarButtons.add(buttonRemove);
    buttonModify = Utilities.createJButton(
        "gnome-accessories-text-editor.png", EnumImageSize.NORMAL,
        GT._("Modify"), false, null);
    buttonModify.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionMdfAutomaticFixing"));
    toolBarButtons.add(buttonModify);
    buttonClear = Utilities.createJButton(
        "gnome-edit-clear.png", EnumImageSize.NORMAL,
        GT._("Clear"), false, null);
    buttonClear.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionClrAutomaticFixing"));
    toolBarButtons.add(buttonClear);
    buttonSave = Utilities.createJButton(
        "gnome-media-floppy.png", EnumImageSize.NORMAL,
        GT._("Save"), false, null);
    buttonSave.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionSaveAutomaticFixing"));
    toolBarButtons.add(buttonSave);    
    toolBarButtons.addSeparator();
    buttonRun = Utilities.createJButton(
        "gnome-system-run.png", EnumImageSize.NORMAL,
        GT._("Fix selected pages"), false, null);
    buttonRun.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionRunAutomaticFixing"));
    toolBarButtons.add(buttonRun);
    buttonTest = Utilities.createJButton(
        "gnome-edit-find.png", EnumImageSize.NORMAL,
        GT._("Test automatic replacements"), false, null);
    buttonTest.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionTestAutomaticFixing"));
    toolBarButtons.add(buttonTest);
    buttonLoadList = Utilities.createJButton(
        "gnome-drive-harddisk.png", EnumImageSize.NORMAL,
        GT._("Load replacements"), false, null);
    buttonLoadList.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionLoadList"));
    toolBarButtons.add(buttonLoadList);
    buttonSaveList = Utilities.createJButton(
        "gnome-document-save.png", EnumImageSize.NORMAL,
        GT._("Save replacements"), false, null);
    buttonSaveList.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionSaveList"));
    toolBarButtons.add(buttonSaveList);
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridwidth = 2;
    constraints.weighty = 0;
    panel.add(toolBarButtons, constraints);
    constraints.gridy++;

    // Apply automatic fixing for CW errors
    automaticCWAlgorithms = new ArrayList<>();
    JPanel panelAutomaticCW = new JPanel(new FlowLayout(FlowLayout.LEADING, 5, 0));
    JLabel labelAutomaticCW = Utilities.createJLabel(GT._("Automatic fixing for Check Wiki"));
    panelAutomaticCW.add(labelAutomaticCW);
    buttonAutomaticCWAlgorithms = Utilities.createJButton("(List of errors)", null);
    buttonAutomaticCWAlgorithms.setBorderPainted(false);
    buttonAutomaticCWAlgorithms.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionAutomaticCWAlgorithms"));
    panelAutomaticCW.add(buttonAutomaticCWAlgorithms);
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridwidth = 2;
    constraints.weighty = 0;
    panel.add(panelAutomaticCW, constraints);
    constraints.gridy++;

    // Force application of fixing for CW errors;
    forceCWAlgorithms = new ArrayList<>();
    JPanel panelForceCW = new JPanel(new FlowLayout(FlowLayout.LEADING, 5, 0));
    JLabel labelForceCW = Utilities.createJLabel(
        GT._("Always apply automatic fixing for Check Wiki"));
    panelForceCW.add(labelForceCW);
    buttonForceCWAlgorithms = Utilities.createJButton("(List of errors)", null);
    buttonForceCWAlgorithms.setBorderPainted(false);
    buttonForceCWAlgorithms.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionForceCWAlgorithms"));
    panelForceCW.add(buttonForceCWAlgorithms);
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridwidth = 2;
    constraints.weighty = 0;
    panel.add(panelForceCW, constraints);
    constraints.gridy++;

    // Wait after each edit
    chkWaitAfterEdit = Utilities.createJCheckBox(
        GT._("Pause after each edit"), true);
    constraints.gridwidth = 2;
    constraints.weighty = 0;
    panel.add(chkWaitAfterEdit, constraints);
    constraints.gridy++;

    // Comment
    constraints.gridwidth = 1;
    addComment(panel, constraints);
    constraints.gridy++;
    constraints.gridx = 0;

    // Automatic fixing list
    modelAutomaticFixing = new AutomaticFixingTableModel(null);
    tableAutomaticFixing = new JTable(modelAutomaticFixing);
    TableColumnModel columnModel = tableAutomaticFixing.getColumnModel();
    columnModel.getColumn(AutomaticFixingTableModel.COLUMN_FROM).setMinWidth(200);
    columnModel.getColumn(AutomaticFixingTableModel.COLUMN_TO).setMinWidth(200);
    columnModel.getColumn(AutomaticFixingTableModel.COLUMN_REGEX).setMinWidth(40);
    columnModel.getColumn(AutomaticFixingTableModel.COLUMN_REGEX).setPreferredWidth(40);
    columnModel.getColumn(AutomaticFixingTableModel.COLUMN_REGEX).setMaxWidth(40);
    JScrollPane scrollAutomaticFixing = new JScrollPane(tableAutomaticFixing);
    scrollAutomaticFixing.setMinimumSize(new Dimension(100, 100));
    scrollAutomaticFixing.setPreferredSize(new Dimension(400, 150));
    scrollAutomaticFixing.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
    constraints.fill = GridBagConstraints.BOTH;
    constraints.gridwidth = 2;
    constraints.weighty = 1;
    panel.add(scrollAutomaticFixing, constraints);
    constraints.gridy++;

    // Test panel
    JPanel testPanel = new JPanel(new GridLayout(1, 0));
    testPanel.setBorder(BorderFactory.createTitledBorder(
        BorderFactory.createEtchedBorder(),
        GT._("Test automatic replacements")));
    paneOriginal = new JTextPane();
    JScrollPane scrollOriginal = new JScrollPane(paneOriginal);
    scrollOriginal.setMinimumSize(new Dimension(50, 50));
    scrollOriginal.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
    testPanel.add(scrollOriginal);
    paneResult = new JTextPane();
    paneResult.setEditable(false);
    JScrollPane scrollResult = new JScrollPane(paneResult);
    scrollResult.setMinimumSize(new Dimension(50, 50));
    scrollResult.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
    testPanel.add(scrollResult);
    constraints.weighty = 0.5;
    panel.add(testPanel, constraints);
    constraints.gridy++;

    // Reset CW selection
    toggleAlgorithm("*", automaticCWAlgorithms);
    toggleAlgorithm("-", forceCWAlgorithms);

    return panel;
  }

  /**
   * @return Links components.
   */
  private Component createLinksComponents() {
    JPanel panel = new JPanel(new GridBagLayout());

    listPages = new JList<Page>(modelPages);

    // Initialize constraints
    GridBagConstraints constraints = new GridBagConstraints();
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridheight = 1;
    constraints.gridwidth = 1;
    constraints.gridx = 0;
    constraints.gridy = 0;
    constraints.insets = new Insets(0, 0, 0, 0);
    constraints.ipadx = 0;
    constraints.ipady = 0;
    constraints.weightx = 1;
    constraints.weighty = 0;

    // Button toolbar
    JToolBar toolbar = new JToolBar(SwingConstants.HORIZONTAL);
    toolbar.setFloatable(false);
    ActionFullAnalysis.addButton(
        getParentComponent(), toolbar, getWikipedia(), listPages, null, true, true);
    ActionDisambiguationAnalysis.addButton(
        getParentComponent(), toolbar, getWikipedia(), listPages, true, true);
    ActionExternalViewer.addButton(
        toolbar, getWikipedia(), listPages, false, true, true);
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.weighty = 0;
    panel.add(toolbar, constraints);
    constraints.gridy++;

    // Pages
    listCellRenderer = new PageListCellRenderer();
    listCellRenderer.showRedirect(true);
    listPages.setCellRenderer(listCellRenderer);
    JScrollPane scrollPages = new JScrollPane(listPages);
    scrollPages.setMinimumSize(new Dimension(100, 100));
    scrollPages.setPreferredSize(new Dimension(200, 500));
    scrollPages.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
    constraints.fill = GridBagConstraints.BOTH;
    constraints.weighty = 1;
    panel.add(scrollPages, constraints);
    constraints.gridy++;

    return panel;
  }


  /**
   * Update status of components
   * @see org.wikipediacleaner.gui.swing.OnePageWindow#updateComponentState()
   */
  @Override
  protected void updateComponentState() {
    super.updateComponentState();
    boolean hasReference = (getPage() != null);
    buttonAdd.setEnabled(true);
    buttonClear.setEnabled(true);
    buttonModify.setEnabled(true);
    buttonRemove.setEnabled(true);
    buttonRun.setEnabled(true);
    buttonSave.setEnabled(hasReference);
    buttonTest.setEnabled(true);
    buttonAutomaticCWAlgorithms.setEnabled(true);
    buttonForceCWAlgorithms.setEnabled(true);

    // Set label on algorithms buttons
    buttonAutomaticCWAlgorithms.setText(computeAlgorithmsLabel(automaticCWAlgorithms));
    buttonForceCWAlgorithms.setText(computeAlgorithmsLabel(forceCWAlgorithms));
  }

  /**
   * @param list List of algorithms.
   * @return Label for the list of algorithms.
   */
  private String computeAlgorithmsLabel(List<CheckErrorAlgorithm> list) {
    if ((list == null) || (list.isEmpty())) {
      return GT._("No errors selected");
    }
    Collections.sort(list, new CheckErrorAlgorithmComparator());
    StringBuilder msg = new StringBuilder();
    for (int i = 0; i < list.size(); i++) {
      int j = i;
      while ((j + 1 < list.size()) &&
             (list.get(j).getErrorNumber() + 1 == list.get(j + 1).getErrorNumber())) {
        j++;
      }
      if (msg.length() > 0) {
        msg.append(", ");
      }
      if (j > i + 1) {
        msg.append(list.get(i).getErrorNumber());
        msg.append("-");
        msg.append(list.get(j).getErrorNumber());
        i = j;
      } else {
        msg.append(list.get(i).getErrorNumber());
      }
    }
    return "(" + msg.toString() + ")";
  }

  /**
   * Clean page. 
   */
  @Override
  protected void clean() {
    modelPages.clear();
    updateComponentState();
  }

  /**
   * @return Currently selected automatic fixing expression.
   */
  protected AutomaticFixing getSelectedAutomaticFixing() {
    int row = tableAutomaticFixing.getSelectedRow();
    if (row < 0) {
      return null;
    }
    return modelAutomaticFixing.getAutomaticFixing(
        Utilities.convertRowIndexToModel(tableAutomaticFixing, row));
  }

  /**
   * Action called when Reload button is pressed. 
   */
  @Override
  protected void actionReload() {
    clean();

    // Fill list of pages
    if (pages != null) {
      modelPages.setElements(pages);
    }
    listPages.clearSelection();
    listPages.setSelectionInterval(0, modelPages.getSize() - 1);

    // Fill list of automatic fixing
    Configuration config = Configuration.getConfiguration();
    Page page = getPage();
    if (page != null) {
      Object[] automaticFixing = config.getPojoArray(
          page.getWikipedia(), Configuration.POJO_AUTOMATIC_FIXING,
          page.getTitle(), AutomaticFixing.class);
      if (automaticFixing != null) {
        List<AutomaticFixing> data = new ArrayList<AutomaticFixing>(automaticFixing.length);
        for (int i = 0; i < automaticFixing.length; i++) {
          if (automaticFixing[i] instanceof AutomaticFixing) {
            data.add((AutomaticFixing) automaticFixing[i]);
          }
        }
        modelAutomaticFixing.setData(data);
      } else {
        modelAutomaticFixing.setData(null);
      }
    }

    // Update components
    updateComponentState();
  }

  /**
   * Action called when Add Automatic Fixing button is pressed.
   */
  public void actionAddAutomaticFixing() {

    AutomaticFixing fixing = new AutomaticFixing();
    if (inputAutomaticFixing(fixing)) {
      modelAutomaticFixing.addAutomaticFixing(fixing);
    }
  }

  /**
   * Action called when Modify Automatic Fixing button is pressed. 
   */
  public void actionMdfAutomaticFixing() {
    AutomaticFixing fixing = getSelectedAutomaticFixing();
    if (fixing == null) {
      return;
    }
    if (inputAutomaticFixing(fixing)) {
      tableAutomaticFixing.repaint();
    }
  }

  /**
   * Display a dialog box to edit an automatic fixing expression.
   * 
   * @param fixing Automatic fixing expression.
   * @return True if the edition has been done.
   */
  private boolean inputAutomaticFixing(AutomaticFixing fixing) {

    // Create a panel for information about an automatic fixing expression.
    JPanel panel = new JPanel(new GridBagLayout());
    GridBagConstraints constraints = new GridBagConstraints();
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridheight = 1;
    constraints.gridwidth = 1;
    constraints.gridx = 0;
    constraints.gridy = 0;
    constraints.insets = new Insets(0, 0, 0, 0);
    constraints.ipadx = 0;
    constraints.ipady = 0;
    constraints.weightx = 1;
    constraints.weighty = 0;

    // Initial text
    JLabel labelOriginal = Utilities.createJLabel(GT._("Text that should be replaced"));
    constraints.weightx = 0;
    panel.add(labelOriginal, constraints);
    constraints.gridx++;
    JTextField textOriginal = Utilities.createJTextField(fixing.getOriginalText(), 50);
    constraints.weightx = 1;
    panel.add(textOriginal, constraints);
    constraints.gridy++;
    constraints.gridx = 0;

    // Replacement text
    JLabel labelReplacement = Utilities.createJLabel(GT._("Text to be used as a replacement"));
    constraints.weightx = 0;
    panel.add(labelReplacement, constraints);
    constraints.gridx++;
    JTextField textReplacement = Utilities.createJTextField(fixing.getReplacementText(), 50);
    constraints.weightx = 1;
    panel.add(textReplacement, constraints);
    constraints.gridy++;
    constraints.gridx = 0;

    // Regular expression
    JCheckBox chkRegex = Utilities.createJCheckBox(GT._("Use regular expressions"), fixing.getRegex());
    constraints.gridwidth = 2;
    constraints.weightx = 1;
    panel.add(chkRegex, constraints);
    constraints.gridwidth = 1;
    constraints.gridy++;

    int result = JOptionPane.showConfirmDialog(
        getParentComponent(), panel, GT._("Automatic fixing"),
        JOptionPane.OK_CANCEL_OPTION, JOptionPane.QUESTION_MESSAGE);
    if (result != JOptionPane.OK_OPTION) {
      return false;
    }
    String originalText = textOriginal.getText();
    if ((originalText == null) || (originalText.length() == 0)) {
      return false;
    }
    String replacementText = textReplacement.getText();
    if (replacementText == null) {
      return false;
    }
    fixing.setOriginalText(originalText);
    fixing.setReplacementText(replacementText);
    fixing.setRegex(chkRegex.isSelected());
    return true;
  }

  /**
   * Action called when Remove Automatic Fixing button is pressed.
   */
  public void ActionRmvAutomaticFixing() {
    AutomaticFixing fixing = getSelectedAutomaticFixing();
    if (fixing != null) {
      modelAutomaticFixing.removeAutomaticFixing(fixing);
    }
  }

  /**
   * Action called when Clear Automatic Fixing button is pressed. 
   */
  public void actionClrAutomaticFixing() {
    modelAutomaticFixing.setData(null);
  }

  /**
   * Action called when Run Automatic Fixing button is pressed. 
   */
  public void actionRunAutomaticFixing() {
    runAutomaticFixing(true);
  }

  /**
   * Action called when Run Automatic Fixing button is pressed.
   * 
   * @param save True if modifications should be saved.
   */
  private void runAutomaticFixing(boolean save) {

    // Check that information is set
    List<Page> values = listPages.getSelectedValuesList();
    if ((values == null) || (values.size() == 0)) {
      Utilities.displayWarning(
          getParentComponent(),
          GT._("You must select the pages on which running automatic fixing."));
      return;
    }
    String comment = getComment();
    if ((comment != null) &&
        (comment.trim().length() == 0)) {
      if (save) {
        Utilities.displayWarning(getParentComponent(), GT._(
            "A comment is required for automatic fixing."));
        return;
      }
      comment = "Test";
    }
    List<AutomaticFixing> fixing = modelAutomaticFixing.getData();
    if ((fixing == null) || (fixing.isEmpty())) {
      if (automaticCWAlgorithms.isEmpty() || forceCWAlgorithms.isEmpty()) {
        Utilities.displayWarning(
            getParentComponent(),
            GT._("You must input the initial and destination texts."));
        return;
      }
    }
    boolean pauseAfterEachEdit = chkWaitAfterEdit.isSelected();

    // Warn the user about what this function does
    if (save) {
      int answer = Utilities.displayYesNoWarning(
          getParentComponent(),
          GT._("!!! WARNING !!!") + "\n" +
          GT._("This function will carry out all replacements on all selected pages.") + "\n" +
          GT._("It may modify a lot of pages in a short period of time.") + "\n" +
          GT._("On some Wikipedia projects, you may need the bot status for doing this.") + "\n" +
          GT._("Please, check if you need the bot status by reading the rules of Wikipedia.") + "\n" +
          GT._("Also, verify again the texts you have inputted before running this function.") + "\n" +
          GT._("You can also test the modifications before actually doing them.") + "\n" +
          GT._("Do you want to proceed with the modifications ?"));
      if (answer != JOptionPane.YES_OPTION) {
        return;
      }
    }

    // Prepare the replacements
    Page[] tmpPages = new Page[values.size()];
    for (int i = 0; i < values.size(); i++) {
      tmpPages[i] = values.get(i);
    }
    Map<String, List<AutomaticFixing>> replacements = new HashMap<String, List<AutomaticFixing>>();
    if (getPage() != null) {
      replacements.put("[[" + getPage().getTitle() + "]]", fixing);
    } else {
      replacements.put(null, fixing);
    }

    // Do the replacements
    AutomaticFixingWorker automaticWorker = new AutomaticFixingWorker(
        getWiki(), this, tmpPages, replacements,
        comment, true,
        automaticCWAlgorithms, forceCWAlgorithms,
        save, pauseAfterEachEdit, this.getParentComponent());
    automaticWorker.setListener(new DefaultBasicWorkerListener() {
      @Override
      public void afterFinished(
          BasicWorker worker,
          @SuppressWarnings("unused") boolean ok) {
        if (!worker.shouldContinue()) {
          return;
        }
        if (worker.get() instanceof Integer) {
          Integer count = (Integer) worker.get();
          if (count.intValue() == 0) {
            return;
          }
        }
        actionReload();
      }
    });
    automaticWorker.start();
  }

  /**
   * Action called when Test Automatic Fixing button is pressed.
   */
  public void actionTestAutomaticFixing() {

    // Test replacements in the test pane
    String text = paneOriginal.getText();
    if ((text != null) && (text.trim().length() > 0)) {
      List<AutomaticFixing> fixing = modelAutomaticFixing.getData();
      List<String> replacements = new ArrayList<String>();
      text = AutomaticFixing.apply(fixing, text, replacements);
      paneResult.setText(text);
      StringBuilder tmp = new StringBuilder();
      tmp.append(GT._("The following replacements have been made:"));
      for (String replacement : replacements) {
        tmp.append("\n - ");
        tmp.append(replacement);
      }
      Utilities.displayInformationMessage(getParentComponent(), tmp.toString());
    }

    // Test replacements in the page list
    List<Page> values = listPages.getSelectedValuesList();
    if ((values != null) && (values.size() > 0)) {
      String message = GT._("Do you want to test the replacements on the pages ?");
      int answer = Utilities.displayYesNoWarning(getParentComponent(), message);
      if (answer == JOptionPane.YES_OPTION) {
        runAutomaticFixing(false);
      }
    }
  }

  /**
   * Action called when Load List button is pressed.
   */
  public void actionLoadList() {
    try {
      JFileChooser fc = new JFileChooser();
      FileFilter filter = new FileNameExtensionFilter(GT._("XML files"), "xml");
      fc.addChoosableFileFilter(filter);
      fc.setFileFilter(filter);
      Configuration config = Configuration.getConfiguration();
      String directory = config.getString(getWiki(), ConfigurationValueString.LAST_REPLACEMENTS_DIRECTORY);
      if (directory != null) {
        fc.setCurrentDirectory(new File(directory));
      }
      int returnVal = fc.showOpenDialog(getParentComponent());
      if (returnVal == JFileChooser.APPROVE_OPTION) {
        File file = fc.getSelectedFile();
        JAXBContext context = JAXBContext.newInstance(AutomaticFixingList.class);
        Unmarshaller um = context.createUnmarshaller();
        AutomaticFixingList list = (AutomaticFixingList) um.unmarshal(new FileReader(file));
        for (AutomaticFixing element : list.getReplacements()) {
          modelAutomaticFixing.addAutomaticFixing(element);
        }
        setComment(list.getComment());
        automaticCWAlgorithms.clear();
        automaticCWAlgorithms.addAll(CheckErrorAlgorithms.convertToAlgorithmList(
            list.getAdditionalAlgorithms(), getWiki()));
        forceCWAlgorithms.clear();
        forceCWAlgorithms.addAll(CheckErrorAlgorithms.convertToAlgorithmList(
            list.getForceAlgorithms(), getWiki()));
        updateComponentState();
      }
    } catch (Exception e) {
      Utilities.displayError(getParentComponent(), e);
    }
  }

  /**
   * Action called when Save List button is pressed.
   */
  public void actionSaveList() {
    try {
      JFileChooser fc = new JFileChooser();
      FileFilter filter = new FileNameExtensionFilter(GT._("XML files"), "xml");
      fc.addChoosableFileFilter(filter);
      fc.setFileFilter(filter);
      Configuration config = Configuration.getConfiguration();
      String directory = config.getString(getWiki(), ConfigurationValueString.LAST_REPLACEMENTS_DIRECTORY);
      if (directory != null) {
        fc.setCurrentDirectory(new File(directory));
      }
      int returnVal = fc.showSaveDialog(getParentComponent());
      if (returnVal == JFileChooser.APPROVE_OPTION) {
        File file = fc.getSelectedFile();

        // Check if file exists
        if (file.exists()) {
          int answer = displayYesNoWarning(
              GT._("This file exists, do you want to overwrite it?"));
          if (answer != JOptionPane.YES_OPTION) {
            return;
          }
        }

        // Save file
        JAXBContext context = JAXBContext.newInstance(AutomaticFixingList.class);
        Marshaller m = context.createMarshaller();
        m.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
        AutomaticFixingList list = new AutomaticFixingList();
        list.setReplacements(modelAutomaticFixing.getData());
        list.setComment(getComment());
        list.setAdditionalAlgorithms(
            CheckErrorAlgorithms.convertToIntegerList(automaticCWAlgorithms));
        list.setForceAlgorithms(
            CheckErrorAlgorithms.convertToIntegerList(forceCWAlgorithms));
        m.marshal(list, file);
        config.setString(
            getWiki(), ConfigurationValueString.LAST_REPLACEMENTS_DIRECTORY,
            file.getParentFile().getAbsolutePath());
      }
    } catch (JAXBException e) {
      Utilities.displayError(getParentComponent(), e);
    }
  }

  /**
   * Action called when Save Automatic Fixing button is pressed. 
   */
  public void actionSaveAutomaticFixing() {
    Configuration config = Configuration.getConfiguration();
    List<AutomaticFixing> fixing = modelAutomaticFixing.getData();
    Object[] replacements = (fixing != null) ? fixing.toArray() : null;
    config.addPojoArray(
        getPage().getWikipedia(), Configuration.POJO_AUTOMATIC_FIXING,
        replacements, getPage().getTitle());
  }

  /**
   * Action called when Select Automatic Algorithms button is pressed.
   */
  public void actionAutomaticCWAlgorithms() {
    displayMenuSelectAlgorithms(
        automaticCWAlgorithms,
        "actionAutomaticCWAlgorithm",
        buttonAutomaticCWAlgorithms);
  }

  /**
   * Action called when Select Force Algorithms button is pressed.
   */
  public void actionForceCWAlgorithms() {
    displayMenuSelectAlgorithms(
        forceCWAlgorithms,
        "actionForceCWAlgorithm",
        buttonForceCWAlgorithms);
  }

  /**
   * Display a menu to select algorithms.
   * 
   * @param list List of already selected algorithms.
   * @param action Name of the method to call.
   * @param button Button where the menu should be displayed.
   */
  private void displayMenuSelectAlgorithms(
      List<CheckErrorAlgorithm> list, String action, JButton button) {
    JPopupMenu popup = new JPopupMenu(GT._("Select errors"));
    List<CheckErrorAlgorithm> allAlgorithms = CheckErrorAlgorithms.getAlgorithms(getWiki());
    if ((allAlgorithms == null) || allAlgorithms.isEmpty()) {
      return;
    }
    JMenuItem menuItem = null;

    // Select all errors
    menuItem = new JMenuItem(GT._("Select all errors"));
    menuItem.setActionCommand("*");
    menuItem.addActionListener(EventHandler.create(
        ActionListener.class, this, action, "actionCommand"));
    popup.add(menuItem);

    // Select no errors
    menuItem = new JMenuItem(GT._("Select no errors"));
    menuItem.setActionCommand("-");
    menuItem.addActionListener(EventHandler.create(
        ActionListener.class, this, action, "actionCommand"));
    popup.add(menuItem);

    // Select individual errors
    final Map<TextAttribute, Boolean> inactiveAttributes = new HashMap<TextAttribute, Boolean>();
    inactiveAttributes.put(TextAttribute.STRIKETHROUGH, TextAttribute.STRIKETHROUGH_ON);
    final int PART_SIZE = 20; 
    int lastPart = -1;
    JMenu subMenu = null;
    for (CheckErrorAlgorithm algorithm : allAlgorithms) {
      if (algorithm != null) {
        int errorNumber = algorithm.getErrorNumber();
        boolean useAlgorithm = false;
        if (errorNumber > 0) {
          useAlgorithm = true;
        }
        if (useAlgorithm) {
          int part = (errorNumber - 1) / PART_SIZE;
          if ((subMenu == null) || (part > lastPart)) {
            int from = (part * PART_SIZE) + 1;
            int to = (part + 1) * PART_SIZE;
            subMenu = new JMenu(GT._(
                "Errors from {0} to {1}",
                new Object[] { Integer.valueOf(from), Integer.valueOf(to) }));
            popup.add(subMenu);

            lastPart = part;
          }
          String label =
            algorithm.getErrorNumberString() + " - " +
            algorithm.getShortDescriptionReplaced();

          menuItem = new JCheckBoxMenuItem(label, list.contains(algorithm));
          if (!CWConfigurationError.isPriorityActive(algorithm.getPriority())) {
            menuItem.setEnabled(false);
            menuItem.setFont(menuItem.getFont().deriveFont(inactiveAttributes));
          } else if (!algorithm.isAvailable()) {
            menuItem.setEnabled(false);
          }
          menuItem.setActionCommand(algorithm.getErrorNumberString());
          menuItem.addActionListener(EventHandler.create(
              ActionListener.class, this, action, "actionCommand"));
          subMenu.add(menuItem);
        }
      }
    }
    popup.show(button, 0, button.getHeight());
  }

  /**
   * Action called when an algorithm is selected to be automatic.
   */
  public void actionAutomaticCWAlgorithm(String error) {
    toggleAlgorithm(error, automaticCWAlgorithms);
  }

  /**
   * Action called when an algorithm is selected to be forced.
   */
  public void actionForceCWAlgorithm(String error) {
    toggleAlgorithm(error, forceCWAlgorithms);
  }

  /**
   * Remove or add an algorithm from the list depending if it's already in the list.
   * 
   * @param error Error number.
   * @param list List of algorithms.
   */
  private void toggleAlgorithm(String error, List<CheckErrorAlgorithm> list) {
    if (error == "*") {

      // Select all errors
      list.clear();
      List<CheckErrorAlgorithm> allAlgorithms = CheckErrorAlgorithms.getAlgorithms(getWiki());
      if (allAlgorithms != null) {
        for (CheckErrorAlgorithm algorithm : allAlgorithms) {
          if (CWConfigurationError.isPriorityActive(algorithm.getPriority())) {
            list.add(algorithm);
          }
        }
      }
    } else if (error == "-") {

      // Select no errors
      list.clear();
    } else {
  
      // Toggle an individual error
      int errorNumber = -1;
      try {
        errorNumber = Integer.parseInt(error);
      } catch (NumberFormatException e) {
        return;
      }
      CheckErrorAlgorithm algorithm = CheckErrorAlgorithms.getAlgorithm(getWiki(), errorNumber);
      if ((algorithm == null) || (list == null)) {
        return;
      }
      if (list.contains(algorithm)) {
        list.remove(algorithm);
      } else {
        list.add(algorithm);
      }
    }
    updateComponentState();
  }
}
