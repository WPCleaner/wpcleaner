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
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionListener;
import java.beans.EventHandler;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.JTextPane;
import javax.swing.JToggleButton;
import javax.swing.JToolBar;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;
import javax.swing.WindowConstants;
import javax.swing.table.TableColumnModel;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.AutomaticFixing;
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
  private JToggleButton buttonAutomaticCW;
  private JCheckBox chkForceCW;
  private JTextPane paneOriginal;
  private JTextPane paneResult;

  JList<Page> listPages;
  PageListModel modelPages;
  private PageListCellRenderer listCellRenderer;

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
    buttonAutomaticCW = Utilities.createJToggleButton(
        "commons-nuvola-web-broom.png", EnumImageSize.NORMAL,
        GT._("Automatic fixing for Check Wiki"), false);
    buttonAutomaticCW.setSelected(true);
    toolBarButtons.add(buttonAutomaticCW);
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
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridwidth = 2;
    constraints.weighty = 0;
    panel.add(toolBarButtons, constraints);
    constraints.gridy++;

    chkForceCW = Utilities.createJCheckBox(
        GT._("Always apply automatic fixing for Check Wiki"), false);
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridwidth = 2;
    constraints.weighty = 0;
    panel.add(chkForceCW, constraints);
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
      if (!buttonAutomaticCW.isSelected() || !chkForceCW.isSelected()) {
        Utilities.displayWarning(
            getParentComponent(),
            GT._("You must input the initial and destination texts."));
        return;
      }
    }

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
    AutomaticFixingWorker dabWorker = new AutomaticFixingWorker(
        getWikipedia(), this, tmpPages, replacements,
        comment, true, buttonAutomaticCW.isSelected(), chkForceCW.isSelected(), save);
    dabWorker.setListener(new DefaultBasicWorkerListener() {
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
    dabWorker.start();
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
}
