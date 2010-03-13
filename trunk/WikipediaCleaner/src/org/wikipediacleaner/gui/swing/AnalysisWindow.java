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
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.JSplitPane;
import javax.swing.ScrollPaneConstants;
import javax.swing.WindowConstants;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.CompositeComparator;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageComparator;
import org.wikipediacleaner.api.data.PageUtilities;
import org.wikipediacleaner.gui.swing.action.SetComparatorAction;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.DefaultBasicWindowListener;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.component.PageListAnalyzeListener;
import org.wikipediacleaner.gui.swing.component.PageListCellRenderer;
import org.wikipediacleaner.gui.swing.component.PageListModel;
import org.wikipediacleaner.gui.swing.component.PageListPopupListener;
import org.wikipediacleaner.gui.swing.worker.FullAnalysisWorker;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;


/**
 * Analysis window.
 */
public class AnalysisWindow extends PageWindow {

  private final static String ACTION_DISAMBIGUATION_LINK  = "DISAMBIGUATION LINK";
  private final static String ACTION_FULL_ANALYSIS_LINK   = "FULL ANALYSIS LINK";
  private final static String ACTION_NEXT_OCCURENCE       = "NEXT OCCURENCE";
  private final static String ACTION_VALIDATE             = "VALIDATE";
  private final static String ACTION_WATCH_LINK           = "WATCH LINK";

  private JButton buttonNext;
  private JButton buttonValidate;

  JList listLinks;
  PageListModel modelLinks;
  private PageListPopupListener popupListenerLinks;
  JCheckBoxMenuItem menuItemShowDisambiguation;
  JCheckBoxMenuItem menuItemShowMissing;
  JCheckBoxMenuItem menuItemShowOther;
  JCheckBoxMenuItem menuItemShowRedirect;
  JCheckBoxMenuItem menuItemCountDisambiguation;
  JCheckBoxMenuItem menuItemCountMissing;
  JCheckBoxMenuItem menuItemCountOther;
  JCheckBoxMenuItem menuItemCountRedirect;
  private JButton buttonFullAnalysisLink;
  private JButton buttonDisambiguationLink;
  private JButton buttonWatchLink;

  ArrayList<Page> knownPages;

  /**
   * Create and display a AnalysisWindow.
   * 
   * @param page Page name.
   * @param knownPages Pages already loaded.
   * @param wikipedia Wikipedia.
   */
  public static void createAnalysisWindow(
      final String page,
      final ArrayList<Page> knownPages,
      final EnumWikipedia wikipedia) {
    createWindow(
        "AnalysisWindow",
        wikipedia,
        WindowConstants.DISPOSE_ON_CLOSE,
        AnalysisWindow.class,
        new DefaultBasicWindowListener() {
          @Override
          public void initializeWindow(BasicWindow window) {
            if (window instanceof AnalysisWindow) {
              Configuration config = Configuration.getConfiguration();
              AnalysisWindow analysis = (AnalysisWindow) window;
              analysis.setPageName(page);
              analysis.knownPages = knownPages;
              analysis.modelLinks = new PageListModel();
              analysis.modelLinks.setShowDisambiguation(config.getBoolean(
                  Configuration.BOOLEAN_ANALYSIS_DISAMBIG_PAGES,
                  Configuration.DEFAULT_ANALYSIS_DISAMBIG_PAGES));
              analysis.modelLinks.setShowMissing(config.getBoolean(
                  Configuration.BOOLEAN_ANALYSIS_MISSING_PAGES,
                  Configuration.DEFAULT_ANALYSIS_MISSING_PAGES));
              analysis.modelLinks.setShowOther(config.getBoolean(
                  Configuration.BOOLEAN_ANALYSIS_OTHER_PAGES,
                  Configuration.DEFAULT_ANALYSIS_OTHER_PAGES));
              analysis.modelLinks.setShowRedirect(config.getBoolean(
                  Configuration.BOOLEAN_ANALYSIS_REDIRECT_PAGES,
                  Configuration.DEFAULT_ANALYSIS_REDIRECT_PAGES));
              analysis.modelLinks.setCountDisambiguation(config.getBoolean(
                  Configuration.BOOLEAN_ANALYSIS_COUNT_DISAMBIG,
                  Configuration.DEFAULT_ANALYSIS_COUNT_DISAMBIG));
              analysis.modelLinks.setCountMissing(config.getBoolean(
                  Configuration.BOOLEAN_ANALYSIS_COUNT_MISSING,
                  Configuration.DEFAULT_ANALYSIS_COUNT_MISSING));
              analysis.modelLinks.setCountOther(config.getBoolean(
                  Configuration.BOOLEAN_ANALYSIS_COUNT_OTHER,
                  Configuration.DEFAULT_ANALYSIS_COUNT_OTHER));
              analysis.modelLinks.setCountRedirect(config.getBoolean(
                  Configuration.BOOLEAN_ANALYSIS_COUNT_REDIRECT,
                  Configuration.DEFAULT_ANALYSIS_COUNT_REDIRECT));
              analysis.modelLinks.setComparator(PageComparator.getNamespaceFirstComparator());
              analysis.createTextContents(window);
            }
          }
          @Override
          public void displayWindow(BasicWindow window) {
            if (window instanceof AnalysisWindow) {
              AnalysisWindow analysis = (AnalysisWindow) window;
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
    return GT._("Analysis - {0}", getPageName());
  }

  /**
   * @return Menu bar.
   */
  @Override
  protected JMenuBar createMenuBar() {
    JMenuBar menuBar = new JMenuBar();
    menuBar.add(createToolsMenu());
    menuBar.add(createOptionsMenu());
    menuBar.add(createSortMenu());
    menuBar.add(Box.createHorizontalGlue());
    JLabel linkCount = new JLabel(GT._("Link count"));
    modelLinks.setLinkCountLabel(linkCount);
    menuBar.add(linkCount);
    return menuBar;
  }

  /**
   * Update component state.
   */
  @Override
  protected void updateComponentState() {
    buttonNext.setEnabled(isPageLoaded());
    buttonValidate.setEnabled(isPageLoaded());
    super.updateComponentState();
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

    // Page name
    //constraints.gridwidth = 2;
    panel.add(createPageComponents(), constraints);
    constraints.gridy++;

    // Contents
    constraints.fill = GridBagConstraints.BOTH;
    constraints.gridwidth = 1;
    constraints.gridx = 0;
    constraints.weightx = 1;
    constraints.weighty = 1;
    JSplitPane split = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
    split.setLeftComponent(createLinksComponents());
    split.setRightComponent(createContentsComponents());
    split.setPreferredSize(new Dimension(1200, 700));
    split.setMinimumSize(new Dimension(200, 200));
    split.setResizeWeight(0.0);
    split.setDividerLocation(200 + split.getInsets().left);
    panel.add(split, constraints);
    constraints.gridy++;

    updateComponentState();
    return panel;
  }

  /**
   * @return Page components.
   */
  private Component createPageComponents() {
    JPanel panel = new JPanel();
    panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
    JPanel panelInformation = new JPanel(new FlowLayout(FlowLayout.CENTER, 5, 0));
    JPanel panelComment = new JPanel(new GridBagLayout());

    // Page name
    addTextPageName(panelInformation);

    // Check box for closing after sending
    addChkCloseAfterSend(panelInformation);

    // Check box for adding a note on the talk page
    addChkEditTalkPage(panelInformation);

    // Comment
    GridBagConstraints constraints = new GridBagConstraints();
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridheight = 1;
    constraints.gridwidth = 1;
    constraints.gridx = 0;
    constraints.gridy = 0;
    constraints.insets = new Insets(0, 0, 0, 0);
    constraints.ipadx = 0;
    constraints.ipady = 0;
    constraints.weightx = 0;
    constraints.weighty = 0;
    addChkAutomaticComment(panelComment, constraints);

    panel.add(panelInformation);
    panel.add(panelComment);

    return panel;
  }

  /**
   * @return Contents components.
   */
  private Component createContentsComponents() {
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

    // Command buttons
    JPanel panelCommand = new JPanel(new FlowLayout(FlowLayout.LEFT, 1, 0));
    addButtonReload(panelCommand);
    addButtonView(panelCommand);
    addButtonSend(panelCommand);
    addButtonWatch(panelCommand);
    addButtonDisambiguation(panelCommand);
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridx = 0;
    constraints.weightx = 1;
    constraints.weighty = 0;
    panel.add(panelCommand);
    constraints.gridy++;

    // Text buttons
    JPanel buttonTextPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 1, 0));
    buttonNext = Utilities.createJButton(GT._("&Next occurrence"));
    buttonNext.setActionCommand(ACTION_NEXT_OCCURENCE);
    buttonNext.addActionListener(this);
    buttonTextPanel.add(buttonNext);
    buttonValidate = Utilities.createJButton(GT._("&Validate"));
    buttonValidate.setActionCommand(ACTION_VALIDATE);
    buttonValidate.addActionListener(this);
    buttonTextPanel.add(buttonValidate);
    addButtonUndoRedo(buttonTextPanel);
    addButtonRedirect(buttonTextPanel);
    addLblLastModified(buttonTextPanel);
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.gridx = 0;
    constraints.weightx = 1;
    constraints.weighty = 0;
    panel.add(buttonTextPanel, constraints);
    constraints.gridy++;

    // Contents
    constraints.fill = GridBagConstraints.BOTH;
    constraints.gridx = 0;
    constraints.weightx = 1;
    constraints.weighty = 1;
    addTextContents(panel, constraints);
    constraints.gridy++;

    return panel;
  }

  /**
   * @return Sort menu.
   */
  private JMenu createSortMenu() {
    JMenu menu = Utilities.createJMenu(GT._("Sort"));
    List<CompositeComparator<Page>> comparators = PageComparator.getComparators();
    for (CompositeComparator<Page> comparator : comparators) {
      JMenuItem menuItem = Utilities.createJMenuItem(comparator.getName());
      menuItem.addActionListener(new SetComparatorAction(modelLinks, comparator));
      menu.add(menuItem);
    }
    return menu;
  }

  /**
   * @return Options menu.
   */
  private JMenu createOptionsMenu() {
    JMenu menu = Utilities.createJMenu(GT._("&Options"));
    menuItemShowDisambiguation = Utilities.createJCheckBoxMenuItm(
        GT._("Show &disambiguation pages"), modelLinks.getShowDisambiguation());
    menuItemShowDisambiguation.addItemListener(this);
    menu.add(menuItemShowDisambiguation);
    menuItemShowMissing = Utilities.createJCheckBoxMenuItm(
        GT._("Show &missing pages"), modelLinks.getShowMissing());
    menuItemShowMissing.addItemListener(this);
    menu.add(menuItemShowMissing);
    menuItemShowRedirect = Utilities.createJCheckBoxMenuItm(
        GT._("Show &redirect pages"), modelLinks.getShowRedirect());
    menuItemShowRedirect.addItemListener(this);
    menu.add(menuItemShowRedirect);
    menuItemShowOther = Utilities.createJCheckBoxMenuItm(
        GT._("Show &other pages"), modelLinks.getShowOther());
    menuItemShowOther.addItemListener(this);
    menu.add(menuItemShowOther);
    menu.add(new JSeparator());
    menuItemCountDisambiguation = Utilities.createJCheckBoxMenuItm(
        GT._("Count disambiguation pages"), modelLinks.getCountDisambiguation());
    menuItemCountDisambiguation.addItemListener(this);
    menu.add(menuItemCountDisambiguation);
    menuItemCountMissing = Utilities.createJCheckBoxMenuItm(
        GT._("Count missing pages"), modelLinks.getCountMissing());
    menuItemCountMissing.addItemListener(this);
    menu.add(menuItemCountMissing);
    menuItemCountRedirect = Utilities.createJCheckBoxMenuItm(
        GT._("Count redirect pages"), modelLinks.getCountRedirect());
    menuItemCountRedirect.addItemListener(this);
    menu.add(menuItemCountRedirect);
    menuItemCountOther = Utilities.createJCheckBoxMenuItm(
        GT._("Count other pages"), modelLinks.getCountOther());
    menuItemCountOther.addItemListener(this);
    menu.add(menuItemCountOther);
    return menu;
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

    // Full analysis button
    buttonFullAnalysisLink = Utilities.createJButton(GT._("&Full analysis"));
    buttonFullAnalysisLink.setActionCommand(ACTION_FULL_ANALYSIS_LINK);
    buttonFullAnalysisLink.addActionListener(this);
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.weightx = 1;
    constraints.weighty = 0;
    panel.add(buttonFullAnalysisLink, constraints);
    constraints.gridy++;

    // Disambiguation button
    buttonDisambiguationLink = Utilities.createJButton(GT._("&Disambiguation"));
    buttonDisambiguationLink.setActionCommand(ACTION_DISAMBIGUATION_LINK);
    buttonDisambiguationLink.addActionListener(this);
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.weightx = 1;
    panel.add(buttonDisambiguationLink, constraints);
    constraints.gridy++;

    // Watch link button
    buttonWatchLink = Utilities.createJButton(GT._("Add to &Watch list"));
    buttonWatchLink.setActionCommand(ACTION_WATCH_LINK);
    buttonWatchLink.addActionListener(this);
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.weightx = 1;
    panel.add(buttonWatchLink, constraints);
    constraints.gridy++;

    // Links
    constraints.fill = GridBagConstraints.BOTH;
    constraints.weighty = 1;
    listLinks = new JList(modelLinks);
    PageListCellRenderer listCellRenderer = new PageListCellRenderer();
    listCellRenderer.showCountOccurence(true);
    listLinks.setCellRenderer(listCellRenderer);
    popupListenerLinks = new PageListPopupListener(getWikipedia(), getTextContents(), this);
    listLinks.addMouseListener(popupListenerLinks);
    listLinks.addMouseListener(new PageListAnalyzeListener(getWikipedia()));
    listLinks.addListSelectionListener(new AnalysisListSelectionListener());
    JScrollPane scrollLinks = new JScrollPane(listLinks);
    scrollLinks.setMinimumSize(new Dimension(100, 100));
    scrollLinks.setPreferredSize(new Dimension(200, 500));
    scrollLinks.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
    panel.add(scrollLinks, constraints);
    constraints.gridy++;

    return panel;
  }

  /**
   * A ListSelectionListener implementation for the links. 
   */
  class AnalysisListSelectionListener implements ListSelectionListener {

    public void valueChanged(ListSelectionEvent e) {
      if (e.getSource() instanceof JList) {
        JList list = (JList) e.getSource();
        Object[] selection = list.getSelectedValues();
        ArrayList<Page> pages = new ArrayList<Page>();
        if (selection != null) {
          for (int i = 0; i < selection.length; i++) {
            if (selection[i] instanceof Page) {
              pages.add((Page) selection[i]);
            }
          }
        }
        ArrayList<Page> oldPages = getTextContents().getInternalLinks();
        if (!pages.equals(oldPages)) {
          getTextContents().setInternalLinks(pages);
        }
      }
    }
  }

  /* (non-Javadoc)
   * @see java.awt.event.ItemListener#itemStateChanged(java.awt.event.ItemEvent)
   */
  @Override
  public void itemStateChanged(ItemEvent e) {
    if ((e == null) || (e.getSource() == null)) {
      return;
    }
    super.itemStateChanged(e);
    Object source = e.getSource();
    boolean selected = (e.getStateChange() == ItemEvent.SELECTED);
    if (source == menuItemShowDisambiguation) {
      modelLinks.setShowDisambiguation(selected);
    } else if (source == menuItemShowMissing) {
      modelLinks.setShowMissing(selected);
    } else if (source == menuItemShowOther) {
      modelLinks.setShowOther(selected);
    } else if (source == menuItemShowRedirect) {
      modelLinks.setShowRedirect(selected);
    } else if (source == menuItemCountDisambiguation) {
      modelLinks.setCountDisambiguation(selected);
    } else if (source == menuItemCountMissing) {
      modelLinks.setCountMissing(selected);
    } else if (source == menuItemCountOther) {
      modelLinks.setCountOther(selected);
    } else if (source == menuItemCountRedirect) {
      modelLinks.setCountRedirect(selected);
    }
  }

  /* (non-Javadoc)
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed(ActionEvent e) {
    if (e == null) {
      return;
    }

    super.actionPerformed(e);
    if (ACTION_VALIDATE.equals(e.getActionCommand())) {
      actionValidate();
    } else if (ACTION_WATCH_LINK.equals(e.getActionCommand())) {
      actionWatchLink();
    } else if (ACTION_DISAMBIGUATION_LINK.equals(e.getActionCommand())) {
      actionDisambiguationLink();
    } else if (ACTION_FULL_ANALYSIS_LINK.equals(e.getActionCommand())) {
      actionFullAnalysisLink();
    } else if (ACTION_NEXT_OCCURENCE.equals(e.getActionCommand())) {
      actionNextOccurence();
    }
  }

  /**
   * Clean page. 
   */
  @Override
  protected void clean() {
    super.clean();
    popupListenerLinks.setPage(getPage());
    modelLinks.clear();
    updateComponentState();
  }

  /**
   * Action called when Reload button is pressed. 
   */
  @Override
  protected void actionReload() {
    clean();
    FullAnalysisWorker reloadWorker = new FullAnalysisWorker(this, getPage(), knownPages);
    knownPages = null;
    setupReloadWorker(reloadWorker);
    reloadWorker.start();
  }

  /**
   * Callback called at the end of the Reload Worker.
   */
  @Override
  protected void afterFinishedReloadWorker() {
    super.afterFinishedReloadWorker();
    Page page = getPage();
    if ((page != null) && (page.getLinks() != null)) {
      ArrayList<Page> links = page.getLinks();
      for (Page p : links) {
        modelLinks.addElement(p);
      }
      countOccurences(page.getContents());
    }
    selectLinks(0);
    modelLinks.updateLinkCount();
    createFixRedirectsMenu();
    updateComponentState();
  }

  /**
   * Callback called before the end of the Reload Worker.
   */
  @Override
  protected void beforeFinishedReloadWorker() {
    super.beforeFinishedReloadWorker();
    modelLinks.setShowDisambiguation(menuItemShowDisambiguation.isSelected());
    modelLinks.setShowMissing(menuItemShowMissing.isSelected());
    modelLinks.setShowOther(menuItemShowOther.isSelected());
    modelLinks.setShowRedirect(menuItemShowRedirect.isSelected());
  }

  /**
   * Callback called before the start of the Reload Worker. 
   */
  @Override
  protected void beforeStartReloadWorker() {
    super.beforeStartReloadWorker();
    modelLinks.setShowDisambiguation(true);
    modelLinks.setShowMissing(true);
    modelLinks.setShowOther(true);
    modelLinks.setShowRedirect(true);
  }

  /**
   * Select indexes.
   * 
   * @param startIndex First index to select.
   */
  void selectLinks(int startIndex) {
    if (modelLinks.getSize() == 0) {
      return;
    }
    Configuration config = Configuration.getConfiguration();
    int maxSelections = config.getInt(
        Configuration.INTEGER_ANALYSIS_NB_PAGES,
        Configuration.DEFAULT_ANALYSIS_NB_PAGES);
    int nbSelections = Math.max(Math.min(maxSelections, modelLinks.getSize() - startIndex), 1);
    startIndex = Math.min(startIndex, modelLinks.getSize() - nbSelections);
    listLinks.getSelectionModel().setSelectionInterval(startIndex, startIndex + nbSelections - 1);
    listLinks.ensureIndexIsVisible(startIndex);
    listLinks.ensureIndexIsVisible(startIndex + nbSelections - 1);
  }

  /**
   * Action called when Watch link button is pressed. 
   */
  private void actionWatchLink() {
    Object[] links = listLinks.getSelectedValues();
    if ((links == null) || (links.length == 0)) {
      return;
    }
    if (displayYesNoWarning(
        GT._("Would you like to add these pages on your local Watch list ?")) == JOptionPane.YES_OPTION) {
      Configuration config = Configuration.getConfiguration();
      ArrayList<String> watch = config.getStringArrayList(Configuration.ARRAY_WATCH_PAGES);
      boolean added = false;
      for (Object link : links) {
        if (!watch.contains(link.toString())) {
          added = true;
          watch.add(link.toString());
        }
      }
      if (added) {
        Collections.sort(watch);
        config.setStringArrayList(Configuration.ARRAY_WATCH_PAGES, watch);
      }
    }
  }

  /**
   * Action called when Disambiguation button is pressed.
   */
  private void actionDisambiguationLink() {
    Controller.runDisambiguationAnalysis(
        getParentComponent(),
        listLinks.getSelectedValues(),
        getWikipedia());
  }

  /**
   * Action called when Full analysis button is pressed.
   */
  private void actionFullAnalysisLink() {
    Controller.runFullAnalysis(
        getParentComponent(),
        listLinks.getSelectedValues(),
        null,
        getWikipedia());
  }

  /**
   * Count pages occurences.
   * 
   * @param text Page text.
   */
  void countOccurences(String text) {
    Page page = getPage();
    if ((page != null) && (page.getLinks() != null)) {
      for (Page p : page.getLinks()) {
        if (p != null) {
          boolean count = false;
          if (Boolean.TRUE.equals(p.isDisambiguationPage())) {
            if (modelLinks.getCountDisambiguation()) {
              count = true;
            }
          } else if (p.isRedirect()) {
            if (modelLinks.getCountRedirect()) {
              count = true;
            }
          } else if (Boolean.FALSE.equals(p.isExisting())) {
            if (modelLinks.getCountMissing()) {
              count = true;
            }
          } else {
            if (modelLinks.getCountOther()) {
              count = true;
            }
          }
          if (count) {
            PageUtilities.countLinkOccurencesInText(getWikipedia(), page, text, p);
          }
        }
      }
    }
  }

  /**
   * Action called when Validate button is pressed.
   */
  private void actionValidate() {
    getTextContents().resetAttributes();
    countOccurences(getTextContents().getText());
    listLinks.repaint();

    // If the selected links are fixed, select the next one
    Object[] values = listLinks.getSelectedValues();
    int count = 0;
    int countElement = 0;
    if (values != null) {
      for (Object value : values) {
        if (value instanceof Page) {
          countElement++;
          count += ((Page) value).getCountOccurence();
        }
      }
    }
    if ((countElement > 0) && (count == 0)) {
      int selected = listLinks.getMaxSelectionIndex();
      selected++;
      if (selected < modelLinks.getSize()) {
        selectLinks(selected);
      }
    }

    modelLinks.updateLinkCount();
    getTextContents().requestFocusInWindow();
  }

  /**
   * Action called when Next Occurence button is pressed. 
   */
  private void actionNextOccurence() {
    getTextContents().selectNextOccurence();
    getTextContents().requestFocusInWindow();
  }
}
