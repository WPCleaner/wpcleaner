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
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import javax.swing.BorderFactory;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JToolBar;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;
import javax.swing.WindowConstants;

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

  private JList listAutomaticFixing;
  private DefaultListModel modelAutomaticFixing;
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
    modelAutomaticFixing = new DefaultListModel();
    listAutomaticFixing = new JList(modelAutomaticFixing);
    listAutomaticFixing.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    JScrollPane scrollAutomaticFixing = new JScrollPane(listAutomaticFixing);
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
        modelAutomaticFixing.clear();
        for (int i = 0; i < automaticFixing.length; i++) {
          modelAutomaticFixing.addElement(automaticFixing[i]);
        }
      }
    }

    // Update components
    updateComponentState();
  }

  /**
   * Action called when Add Automatic Fixing button is pressed.
   */
  public void actionAddAutomaticFixing() {
    String initialText = Utilities.askForValue(
        getParentComponent(), "Input the text that should be replaced", "", null);
    if ((initialText == null) || (initialText.length() == 0)) {
      return;
    }
    String replaceText = Utilities.askForValue(
        getParentComponent(), "Input the text that should be used as replacement", "", null);
    if ((replaceText == null) || (replaceText.length() == 0)) {
      return;
    }
    modelAutomaticFixing.addElement(new AutomaticFixing(initialText, replaceText));
  }

  /**
   * Action called when Modify Automatic Fixing button is pressed. 
   */
  public void actionMdfAutomaticFixing() {
    Object selected = listAutomaticFixing.getSelectedValue();
    if (selected instanceof AutomaticFixing) {
      AutomaticFixing fixing = (AutomaticFixing) selected;
      String initialText = Utilities.askForValue(
          getParentComponent(), "Input the text that should be replaced",
          fixing.getOriginalText(), null);
      if ((initialText == null) || (initialText.length() == 0)) {
        return;
      }
      fixing.setOriginalText(initialText);
      String replaceText = Utilities.askForValue(
          getParentComponent(), "Input the text that should be used as replacement",
          fixing.getReplacementText(), null);
      if ((replaceText == null) || (replaceText.length() == 0)) {
        return;
      }
      fixing.setReplacementText(replaceText);
      listAutomaticFixing.repaint();
    }
  }

  /**
   * Action called when Remove Automatic Fixing button is pressed.
   */
  public void ActionRmvAutomaticFixing() {
    int selected = listAutomaticFixing.getSelectedIndex();
    if (selected != - 1) {
      modelAutomaticFixing.remove(selected);
    }
  }

  /**
   * Action called when Clear Automatic Fixing button is pressed. 
   */
  public void actionClrAutomaticFixing() {
    modelAutomaticFixing.clear();
  }

  /**
   * Action called when Run Automatic Fixing button is pressed. 
   */
  public void actionRunAutomaticFixing() {
    Object[] values = listPages.getSelectedValues();
    if ((values == null) || (values.length == 0)) {
      Utilities.displayWarning(
          getParentComponent(),
          GT._("You must select the pages on which running automatic fixing."));
      return;
    }
    if (modelAutomaticFixing.isEmpty()) {
      Utilities.displayWarning(
          getParentComponent(),
          GT._("You must input the initial and destination texts."));
      return;
    }
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
    Page[] tmpPages = new Page[values.length];
    for (int i = 0; i < values.length; i++) {
      tmpPages[i] = (Page) values[i];
    }
    Properties replacement = new Properties();
    for (int i = 0; i < modelAutomaticFixing.getSize(); i++) {
      Object value = modelAutomaticFixing.get(i);
      if (value instanceof AutomaticFixing) {
        AutomaticFixing replacementValue = (AutomaticFixing) value;
        replacement.setProperty(
            replacementValue.getOriginalText(),
            replacementValue.getReplacementText());
      }
    }
    Map<String, Properties> replacements = new HashMap<String, Properties>();
    if (getPage() != null) {
      replacements.put("[[" + getPage().getTitle() + "]]", replacement);
    } else {
      replacements.put(null, replacement);
    }
    // Check that a comment is available
    String comment = getComment();
    if ((comment != null) &&
        (comment.trim().length() == 0)) {
      Utilities.displayWarning(getParentComponent(), GT._(
          "A comment is required for automatix fixing."));
      return;
    }
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
    Object[] replacements = modelAutomaticFixing.toArray();
    Arrays.sort(replacements, new Comparator<Object>() {

      public int compare(Object o1, Object o2) {
        AutomaticFixing a1 = (o1 instanceof AutomaticFixing) ? (AutomaticFixing) o1 : null;
        AutomaticFixing a2 = (o2 instanceof AutomaticFixing) ? (AutomaticFixing) o2 : null;
        if (a2 == null) {
          if (a1 == null) {
            return 0;
          }
          return -1;
        }
        if (a1 == null) {
          return 1;
        }
        return a1.getOriginalText().compareTo(a2.getOriginalText());
      }
      
    });
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
