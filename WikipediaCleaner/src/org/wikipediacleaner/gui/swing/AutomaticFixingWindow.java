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

import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
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
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.JToolBar;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;
import javax.swing.WindowConstants;
import javax.swing.table.TableColumnModel;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.AutomaticFixing;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageComparator;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.gui.swing.basic.DefaultBasicWindowListener;
import org.wikipediacleaner.gui.swing.basic.DefaultBasicWorkerListener;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.component.PageListCellRenderer;
import org.wikipediacleaner.gui.swing.component.PageListModel;
import org.wikipediacleaner.gui.swing.worker.AutomaticDisambiguationWorker;
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
  private JButton buttonAddAutomaticFixing;
  private JButton buttonMdfAutomaticFixing;
  private JButton buttonRmvAutomaticFixing;
  private JButton buttonClrAutomaticFixing;
  private JButton buttonRunAutomaticFixing;
  private JButton buttonSaveAutomaticFixing;

  JList listPages;
  PageListModel modelPages;
  private PageListCellRenderer listCellRenderer;
  private JButton buttonFullAnalysisLink;
  private JButton buttonDisambiguationLink;
  private JButton buttonExternalViewerLink;

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
    panel.setBorder(BorderFactory.createTitledBorder(
        BorderFactory.createEtchedBorder(),
        GT._("Automatic fixing")));

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

    // Commands
    JToolBar toolBarButtons = new JToolBar(SwingConstants.HORIZONTAL);
    toolBarButtons.setFloatable(false);
    buttonAddAutomaticFixing = Utilities.createJButton(
        "gnome-list-add.png", EnumImageSize.NORMAL,
        GT._("Add"), false);
    buttonAddAutomaticFixing.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionAddAutomaticFixing"));
    toolBarButtons.add(buttonAddAutomaticFixing);
    buttonRmvAutomaticFixing = Utilities.createJButton(
        "gnome-list-remove.png", EnumImageSize.NORMAL,
        GT._("Remove"), false);
    buttonRmvAutomaticFixing.addActionListener(EventHandler.create(
        ActionListener.class, this, "ActionRmvAutomaticFixing"));
    toolBarButtons.add(buttonRmvAutomaticFixing);
    buttonMdfAutomaticFixing = Utilities.createJButton(
        "gnome-accessories-text-editor.png", EnumImageSize.NORMAL,
        GT._("Modify"), false);
    buttonMdfAutomaticFixing.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionMdfAutomaticFixing"));
    toolBarButtons.add(buttonMdfAutomaticFixing);
    buttonClrAutomaticFixing = Utilities.createJButton(
        "gnome-edit-clear.png", EnumImageSize.NORMAL,
        GT._("Clear"), false);
    buttonClrAutomaticFixing.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionClrAutomaticFixing"));
    toolBarButtons.add(buttonClrAutomaticFixing);
    buttonSaveAutomaticFixing = Utilities.createJButton(
        "gnome-media-floppy.png", EnumImageSize.NORMAL,
        GT._("Save"), false);
    buttonSaveAutomaticFixing.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionSaveAutomaticFixing"));
    toolBarButtons.add(buttonSaveAutomaticFixing);
    buttonRunAutomaticFixing = Utilities.createJButton(
        "gnome-system-run.png", EnumImageSize.NORMAL,
        GT._("Fix selected pages"), false);
    buttonRunAutomaticFixing.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionRunAutomaticFixing"));
    toolBarButtons.add(buttonRunAutomaticFixing);
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridwidth = 2;
    constraints.weighty = 0;
    panel.add(toolBarButtons, constraints);
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
    constraints.insets = new Insets(0, 0, 0, 0);
    constraints.ipadx = 0;
    constraints.ipady = 0;
    constraints.weightx = 1;
    constraints.weighty = 0;

    // Button toolbar
    JToolBar toolbar = new JToolBar(SwingConstants.HORIZONTAL);
    toolbar.setFloatable(false);
    buttonFullAnalysisLink = Utilities.createJButton(
        "gnome-system-run.png", EnumImageSize.NORMAL,
        GT._("Full analysis (Alt + &F)"), false);
    buttonFullAnalysisLink.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionFullAnalysisLink"));
    toolbar.add(buttonFullAnalysisLink);
    buttonDisambiguationLink = Utilities.createJButton(
        "commons-disambig-colour.png", EnumImageSize.NORMAL,
        GT._("Disambiguation (Alt + &D)"), false);
    buttonDisambiguationLink.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionDisambiguationLink"));
    toolbar.add(buttonDisambiguationLink);
    buttonExternalViewerLink = Utilities.createJButton(
        "gnome-emblem-web.png", EnumImageSize.NORMAL,
        GT._("External Viewer"), false);
    buttonExternalViewerLink.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionExternalViewerLink"));
    toolbar.add(buttonExternalViewerLink);
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.weighty = 0;
    panel.add(toolbar, constraints);
    constraints.gridy++;

    // Pages
    listPages = new JList(modelPages);
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
    buttonAddAutomaticFixing.setEnabled(true);
    buttonClrAutomaticFixing.setEnabled(true);
    buttonMdfAutomaticFixing.setEnabled(true);
    buttonRmvAutomaticFixing.setEnabled(true);
    buttonRunAutomaticFixing.setEnabled(true);
    buttonSaveAutomaticFixing.setEnabled(hasReference);
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
    JLabel labelReplacement = Utilities.createJLabel(GT._("Text that should be used as replacement"));
    constraints.weightx = 0;
    panel.add(labelReplacement, constraints);
    constraints.gridx++;
    JTextField textReplacement = Utilities.createJTextField(fixing.getReplacementText(), 50);
    constraints.weightx = 1;
    panel.add(textReplacement, constraints);
    constraints.gridy++;
    constraints.gridx = 0;

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

    // Check that information is set
    Object[] values = listPages.getSelectedValues();
    if ((values == null) || (values.length == 0)) {
      Utilities.displayWarning(
          getParentComponent(),
          GT._("You must select the pages on which running automatic fixing."));
      return;
    }
    String comment = getComment();
    if ((comment != null) &&
        (comment.trim().length() == 0)) {
      Utilities.displayWarning(getParentComponent(), GT._(
          "A comment is required for automatix fixing."));
      return;
    }
    List<AutomaticFixing> fixing = modelAutomaticFixing.getData();
    if ((fixing == null) || (fixing.isEmpty())) {
      Utilities.displayWarning(
          getParentComponent(),
          GT._("You must input the initial and destination texts."));
      return;
    }

    // Warn the user about what this function does
    int answer = Utilities.displayYesNoWarning(
        getParentComponent(),
        GT._("!!! WARNING !!!") + "\n" +
        GT._("This function will do all the replacements in all selected pages.") + "\n" +
        GT._("It may modify a lot of pages in a short period of time.") + "\n" +
        GT._("On some Wikipedia projects, you may need the bot status for doing this.") + "\n" +
        GT._("Please, check if you need the bot status by reading the rules of Wikipedia.") + "\n" +
        GT._("Also, verify again the texts you have inputed before running this function.") + "\n" +
        GT._("Do you want to continue ?"));
    if (answer != JOptionPane.YES_OPTION) {
      return;
    }

    // Prepare the replacements
    Page[] tmpPages = new Page[values.length];
    for (int i = 0; i < values.length; i++) {
      tmpPages[i] = (Page) values[i];
    }
    Map<String, List<AutomaticFixing>> replacements = new HashMap<String, List<AutomaticFixing>>();
    if (getPage() != null) {
      replacements.put("[[" + getPage().getTitle() + "]]", fixing);
    } else {
      replacements.put(null, fixing);
    }

    // Do the replacements
    AutomaticDisambiguationWorker dabWorker = new AutomaticDisambiguationWorker(
        getWikipedia(), this, tmpPages, replacements,
        comment, true);
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
   * Action called when Full analysis button is pressed.
   */
  public void actionFullAnalysisLink() {
    List<Page> knownPages = null;
    if (getPage() != null) {
      knownPages = new ArrayList<Page>(1);
      knownPages.add(getPage());
      for (Page backLink : getPage().getBackLinksWithRedirects()) {
        if ((backLink != null) &&
            (backLink.isRedirect()) &&
            (Page.areSameTitle(getPage().getTitle(), backLink.getRedirectDestination()))) {
          knownPages.add(backLink);
        }
      }
    }
    Controller.runFullAnalysis(
        getParentComponent(), listPages.getSelectedValues(), knownPages, getWikipedia());
  }

  /**
   * Action called when Disambiguation button is pressed.
   */
  public void actionDisambiguationLink() {
    Controller.runDisambiguationAnalysis(
        getParentComponent(), listPages.getSelectedValues(), getWikipedia());
  }

  /**
   * Action called when External Viewer button is pressed.
   */
  public void actionExternalViewerLink() {
    for (Object selection : listPages.getSelectedValues()) {
      if (selection instanceof Page) {
        Utilities.browseURL(getWikipedia(), ((Page) selection).getTitle(), false);
      }
    }
  }
}
