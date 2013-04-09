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
import java.awt.event.ActionListener;
import java.beans.EventHandler;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Properties;

import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JToolBar;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;
import javax.swing.WindowConstants;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.CompositeComparator;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageComparator;
import org.wikipediacleaner.gui.swing.action.SetComparatorAction;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.DefaultBasicWindowListener;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.component.DisambiguationPageListPopupListener;
import org.wikipediacleaner.gui.swing.component.PageListAnalyzeListener;
import org.wikipediacleaner.gui.swing.component.PageListCellRenderer;
import org.wikipediacleaner.gui.swing.component.PageListModel;
import org.wikipediacleaner.gui.swing.worker.DisambiguationAnalysisWorker;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueBoolean;
import org.wikipediacleaner.utils.ConfigurationValueInteger;


/**
 * Disambiguation window.
 */
public class DisambiguationWindow extends OnePageWindow {

  //public final static Integer WINDOW_VERSION = Integer.valueOf(2);

  private Properties backlinksProperties;

  JList listLinks;
  PageListModel modelLinks;
  private PageListCellRenderer listCellRenderer;
  private DisambiguationPageListPopupListener popupListenerLinks;
  JLabel linkCount;
  private JButton buttonFullAnalysisLink;
  private JButton buttonDisambiguationLink;
  private JButton buttonMarkNeedHelp;
  private JButton buttonMarkNormal;
  private JButton buttonExternalViewerLink;
  private JButton buttonSelectNextLinks;
  private JButton buttonAutomaticFixing;

  /**
   * Create and display a DisambiguationWindow.
   * 
   * @param page Page name.
   * @param wikipedia Wikipedia.
   */
  public static void createDisambiguationWindow(
      final String page,
      final EnumWikipedia wikipedia) {
    createWindow(
        "DisambiguationWindow",
        wikipedia,
        WindowConstants.DISPOSE_ON_CLOSE,
        DisambiguationWindow.class,
        new DefaultBasicWindowListener() {
          @Override
          public void initializeWindow(BasicWindow window) {
            if (window instanceof DisambiguationWindow) {
              DisambiguationWindow disambig = (DisambiguationWindow) window;
              disambig.setPageName(page);
              disambig.modelLinks = new PageListModel();
              disambig.modelLinks.setComparator(PageComparator.getTemplateFirstComparator());
              disambig.modelLinks.setShowDisambiguation(true);
              disambig.modelLinks.setShowOther(true);
            }
          }
          @Override
          public void displayWindow(BasicWindow window) {
            if (window instanceof DisambiguationWindow) {
              DisambiguationWindow disambig = (DisambiguationWindow) window;
              disambig.actionReload();
            }
          }
        });
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.basic.BasicWindow#getTitle()
   */
  @Override
  public String getTitle() {
    return GT._("Disambiguation - {0}", getPageName());
  }

  /**
   * @return Menu bar.
   */
  @Override
  protected JMenuBar createMenuBar() {
    JMenuBar menuBar = new JMenuBar();
    menuBar.add(createToolsMenu());
    menuBar.add(createSortMenu());
    menuBar.add(Box.createHorizontalGlue());
    return menuBar;
  }

  /**
   * @return Window components.
   */
  @Override
  protected Component createComponents() {
    createElements();
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
   * Creates internal elements. 
   */
  private void createElements() {
    createTextContents(this);
  }

  /**
   * @return Page components.
   */
  private Component createPageComponents() {
    JPanel panel = new JPanel(new FlowLayout(FlowLayout.CENTER));
    addTextPageName(panel);
    JToolBar toolbar = new JToolBar(SwingConstants.HORIZONTAL);
    toolbar.setFloatable(false);
    addButtonReload(toolbar, true);
    addButtonView(toolbar, true);
    addButtonSend(toolbar, true);
    addButtonWatch(toolbar, true);
    addButtonFullAnalysis(toolbar, true);
    panel.add(toolbar);
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

    // Contents
    constraints.fill = GridBagConstraints.BOTH;
    constraints.weighty = 1;
    addTextContents(panel, constraints);
    constraints.gridy++;

    return panel;
  }

  /**
   * @return Links components.
   */
  private Component createLinksComponents() {
    JPanel panel = new JPanel(new GridBagLayout());
    Configuration configuration = Configuration.getConfiguration();

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

    // Select next links button
    buttonSelectNextLinks = Utilities.createJButton(GT._("Select &next links"));
    buttonSelectNextLinks.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionSelectNextLinks"));
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.weightx = 1;
    panel.add(buttonSelectNextLinks, constraints);
    constraints.gridy++;

    // Automatic fixing
    buttonAutomaticFixing = Utilities.createJButton(GT._("Automatic fixing"));
    buttonAutomaticFixing.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionRunAutomaticFixing"));
    panel.add(buttonAutomaticFixing, constraints);
    constraints.gridy++;

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
    buttonMarkNormal = Utilities.createJButton(
        "wpc-mark-normal.png", EnumImageSize.NORMAL,
        GT._("Mark backlink as normal"), false);
    buttonMarkNormal.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionMarkBacklinkNormal"));
    toolbar.add(buttonMarkNormal);
    buttonMarkNeedHelp = Utilities.createJButton(
        "wpc-mark-need-help.png", EnumImageSize.NORMAL,
        GT._("Mark backlink as needing help"), false);
    buttonMarkNeedHelp.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionMarkBacklinkHelpNeeded"));
    toolbar.add(buttonMarkNeedHelp);
    buttonExternalViewerLink = Utilities.createJButton(
        "gnome-emblem-web.png", EnumImageSize.NORMAL,
        GT._("External Viewer"), false);
    buttonExternalViewerLink.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionExternalViewerLink"));
    toolbar.add(buttonExternalViewerLink);
    toolbar.addSeparator();
    linkCount = new JLabel(GT._("Link count"));
    toolbar.add(linkCount);
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.weightx = 1;
    constraints.weighty = 0;
    panel.add(toolbar, constraints);
    constraints.gridy++;

    // Links
    listLinks = new JList(modelLinks);
    listCellRenderer = new PageListCellRenderer();
    listCellRenderer.showRedirect(true);
    listCellRenderer.showRedirectBacklinks(true);
    if (getPage() != null) {
      listCellRenderer.setPageProperties(configuration.getSubProperties(
          getWikipedia(), Configuration.PROPERTIES_BACKLINKS, getPage().getTitle()));
    }
    listLinks.setCellRenderer(listCellRenderer);
    popupListenerLinks = new DisambiguationPageListPopupListener(
        getWikipedia(), getTextContents(), listLinks, this);
    listLinks.addMouseListener(popupListenerLinks);
    listLinks.addMouseListener(new PageListAnalyzeListener(getWikipedia(), this));
    JScrollPane scrollLinks = new JScrollPane(listLinks);
    scrollLinks.setMinimumSize(new Dimension(100, 100));
    scrollLinks.setPreferredSize(new Dimension(200, 500));
    scrollLinks.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
    constraints.fill = GridBagConstraints.BOTH;
    constraints.weighty = 1;
    panel.add(scrollLinks, constraints);
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
   * Clean page. 
   */
  @Override
  protected void clean() {
    super.clean();
    modelLinks.clear();
    updateComponentState();
  }

  /**
   * Action called when Reload button is pressed. 
   */
  @Override
  protected void actionReload() {
    clean();
    DisambiguationAnalysisWorker reloadWorker = new DisambiguationAnalysisWorker(
        getWikipedia(), this, getPage());
    setupReloadWorker(reloadWorker);
    reloadWorker.start();
  }

  /**
   * Callback called at the end of the Reload Worker.
   */
  @Override
  protected void afterFinishedReloadWorker() {
    super.afterFinishedReloadWorker();
    Configuration config = Configuration.getConfiguration();
    Page page = getPage();
    backlinksProperties = config.getSubProperties(
        getWikipedia(), Configuration.PROPERTIES_BACKLINKS, page.getTitle());
    listCellRenderer.setPageProperties(backlinksProperties);
    popupListenerLinks.setPage(page);
    popupListenerLinks.setBackLinksProperties(backlinksProperties);
    List<Page> links = page.getBackLinksWithRedirects();
    if (config.getBoolean(null, ConfigurationValueBoolean.IGNORE_DAB_USER_NS)) {
      links = new ArrayList<Page>(links);
      for (int i = links.size(); i > 0; i--) {
        if (links.get(i - 1).isInUserNamespace()) {
          links.remove(i - 1);
        }
      }
    }
    modelLinks.setElements(links);
    Integer countMain = page.getBacklinksCountInMainNamespace();
    Integer countTotal = page.getBacklinksCount();
    linkCount.setText(
        ((countMain != null) ? countMain.toString() : "?") +
        " / " +
        ((countTotal != null) ? countTotal.toString() : "?"));

    // Select next links
    actionSelectNextLinks();
  }

  /**
   * Callback called before the start of the Reload Worker. 
   */
  @Override
  protected void beforeStartReloadWorker() {
    super.beforeStartReloadWorker();
    modelLinks.setShowDisambiguation(true);
    modelLinks.setShowOther(true);
  }

  /**
   * Action called when Run Automatic Fixing button is pressed. 
   */
  public void actionRunAutomaticFixing() {
    Object[] values = listLinks.getSelectedValues();
    if ((values == null) || (values.length == 0)) {
      Utilities.displayWarning(
          getParentComponent(),
          GT._("You must select pages on which running automatic fixing."));
      return;
    }
    Collection<Page> pages = new ArrayList<Page>(values.length);
    for (int i = 0; i < values.length; i++) {
      pages.add((Page) values[i]);
    }
    Controller.runAutomatixFixing(pages, getPage(), getWikipedia());
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
        getParentComponent(), listLinks.getSelectedValues(), knownPages, getWikipedia());
  }

  /**
   * Action called when Disambiguation button is pressed.
   */
  public void actionDisambiguationLink() {
    Controller.runDisambiguationAnalysis(
        getParentComponent(), listLinks.getSelectedValues(), getWikipedia());
  }

  /**
   * Action called when Mark back link button is pressed.
   */
  public void actionMarkBacklinkNormal() {
    actionMarkBacklink(Configuration.VALUE_PAGE_NORMAL);
  }

  /**
   * Action called when Mark back link button is pressed.
   */
  public void actionMarkBacklinkHelpNeeded() {
    actionMarkBacklink(Configuration.VALUE_PAGE_HELP_NEEDED);
  }

  /**
   * Action called when Mark back link button is pressed.
   */
  private void actionMarkBacklink(String mark) {
    for (Object selection : listLinks.getSelectedValues()) {
      if (selection instanceof Page) {
        Page selected = (Page) selection;
        backlinksProperties.put(selected.getTitle(), mark);
      }
    }
    Configuration configuration = Configuration.getConfiguration();
    configuration.setSubProperties(
        getWikipedia(),
        Configuration.PROPERTIES_BACKLINKS, getPageName(),
        backlinksProperties);
    listLinks.repaint();
  }

  /**
   * Action called when External Viewer button is pressed.
   */
  public void actionExternalViewerLink() {
    for (Object selection : listLinks.getSelectedValues()) {
      if (selection instanceof Page) {
        Utilities.browseURL(getWikipedia(), ((Page) selection).getTitle(), false);
      }
    }
  }

  /**
   * Action called when Select next links button is pressed. 
   */
  public void actionSelectNextLinks() {

    // Test
    if (listLinks.getModel().getSize() == 0) {
      return;
    }

    // Check if something can be selected after current selection
    int lastSelected = listLinks.getMaxSelectionIndex();
    int currentLine = lastSelected + 1;
    boolean lineAvailable = false;
    while ((!lineAvailable) &&
           (currentLine < listLinks.getModel().getSize())) {
      Object value = listLinks.getModel().getElementAt(currentLine);
      String property = backlinksProperties.getProperty(value.toString());
      if ((!Configuration.VALUE_PAGE_NORMAL.equals(property)) &&
          (!Configuration.VALUE_PAGE_HELP_NEEDED.equals(property))) {
        lineAvailable = true;
      }
      currentLine++;
    }
    if (!lineAvailable) {
      lastSelected = -1;
    }

    // Find first item to be selected
    currentLine = lastSelected + 1;
    int firstSelection = -1;
    Integer firstNamespace = null;
    while ((firstSelection < 0) &&
           (currentLine < listLinks.getModel().getSize())) {
      Object value = listLinks.getModel().getElementAt(currentLine);
      String property = backlinksProperties.getProperty(value.toString());
      if ((!Configuration.VALUE_PAGE_NORMAL.equals(property)) &&
          (!Configuration.VALUE_PAGE_HELP_NEEDED.equals(property))) {
        firstSelection = currentLine;
        if (value instanceof Page) {
          Page firstPage = (Page) value;
          firstNamespace = firstPage.getNamespace();
        }
      }
      currentLine++;
    }
    if (firstSelection < 0) {
      listLinks.clearSelection();
      listLinks.ensureIndexIsVisible(0);
      return;
    }

    // Initialize array for items to be selected
    Configuration config = Configuration.getConfiguration();
    int maxCount = config.getInt(
        null,
        ConfigurationValueInteger.MAXIMUM_PAGES);
    maxCount = Math.min(maxCount, listLinks.getModel().getSize() - firstSelection);
    if (maxCount <= 0) {
      listLinks.clearSelection();
      listLinks.ensureIndexIsVisible(0);
      return;
    }
    int indices[] = new int[maxCount];

    // Find items to be selected
    currentLine = firstSelection;
    int currentIndice = 0;
    while ((currentIndice < maxCount) &&
           (currentLine < listLinks.getModel().getSize())) {
      Object value = listLinks.getModel().getElementAt(currentLine);
      String property = backlinksProperties.getProperty(value.toString());
      if ((firstNamespace == null) ||
          ((value instanceof Page) &&
           (firstNamespace.equals(((Page) value).getNamespace())))) {
        if ((!Configuration.VALUE_PAGE_NORMAL.equals(property)) &&
            (!Configuration.VALUE_PAGE_HELP_NEEDED.equals(property))) {
          indices[currentIndice] = currentLine;
          currentIndice++;
        }
        currentLine++;
      } else {
        currentLine = Integer.MAX_VALUE;
      }
    }

    // Select items found
    if (currentIndice < indices.length) {
      int newIndices[] = new int[currentIndice];
      for (int i = 0; i < currentIndice; i++) {
        newIndices[i] = indices[i];
      }
      indices = newIndices;
    }
    listLinks.setSelectedIndices(indices);
    if (indices.length > 0) {
      listLinks.ensureIndexIsVisible(indices[0]);
      listLinks.ensureIndexIsVisible(indices[indices.length - 1]);
    }
  }
}
