/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.options;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionListener;
import java.beans.EventHandler;
import java.util.LinkedList;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JToolBar;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import org.wikipediacleaner.api.data.CompositeComparator;
import org.wikipediacleaner.api.data.NamedComparator;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageComparator;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;


/**
 * A panel for sorting options.
 */
public class SortingOptionsPanel
  extends OptionsPanel
  implements ListSelectionListener {

  /**
   * Serialisation.
   */
  private static final long serialVersionUID = 2014796573945564540L;

  private JButton buttonSortAdd;
  private JButton buttonSortDelete;
  private JButton buttonSortUp;
  private JButton buttonSortDown;

  private JList<CompositeComparator<Page>> listSort;
  private DefaultListModel<CompositeComparator<Page>> modelSort;
  private JList<NamedComparator<Page>> listSortItem;
  private DefaultListModel<NamedComparator<Page>> modelSortItem;

  /**
   * Construct a General Options panel. 
   */
  public SortingOptionsPanel() {
    super(new GridBagLayout());
    initialize();
  }

  /**
   * Initialize the panel.
   */
  private void initialize() {
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

    // Sort orders
    JPanel panelSortOrders = new JPanel(new GridBagLayout());
    panelSortOrders.setBorder(BorderFactory.createTitledBorder(
        BorderFactory.createEtchedBorder(), GT._("Sort orders")));
    constraints.fill = GridBagConstraints.BOTH;
    constraints.weighty = 1;
    modelSort = new DefaultListModel<CompositeComparator<Page>>();
    List<CompositeComparator<Page>> comparators = PageComparator.getComparators();
    for (CompositeComparator<Page> comparator : comparators) {
      modelSort.addElement(comparator);
    }
    listSort = new JList<CompositeComparator<Page>>(modelSort);
    listSort.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    listSort.addListSelectionListener(this);
    JScrollPane scrollSort = new JScrollPane(listSort);
    scrollSort.setMinimumSize(new Dimension(100, 100));
    scrollSort.setPreferredSize(new Dimension(150, 200));
    scrollSort.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
    panelSortOrders.add(scrollSort, constraints);
    JToolBar toolbarButtons = new JToolBar(SwingConstants.HORIZONTAL);
    toolbarButtons.setFloatable(false);
    buttonSortAdd = Utilities.createJButton(
        "gnome-list-add.png", EnumImageSize.NORMAL, GT._("Add"), false, null);
    buttonSortAdd.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionSortAdd"));
    toolbarButtons.add(buttonSortAdd);
    buttonSortDelete = Utilities.createJButton(
        "gnome-list-remove.png", EnumImageSize.NORMAL, GT._("Delete"), false, null);
    buttonSortDelete.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionSortDelete"));
    toolbarButtons.add(buttonSortDelete);
    constraints.gridy++;
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.weighty = 0;
    panelSortOrders.add(toolbarButtons, constraints);
    constraints.gridy++;

    // Sort description
    JPanel panelSortDescription = new JPanel(new GridBagLayout());
    panelSortDescription.setBorder(BorderFactory.createTitledBorder(
        BorderFactory.createEtchedBorder(), GT._("Details")));
    constraints.gridy = 0;
    constraints.fill = GridBagConstraints.BOTH;
    constraints.weighty = 1;
    modelSortItem = new DefaultListModel<NamedComparator<Page>>();
    listSortItem = new JList<NamedComparator<Page>>(modelSortItem);
    listSortItem.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    JScrollPane scrollSortItem = new JScrollPane(listSortItem);
    scrollSortItem.setMinimumSize(new Dimension(100, 100));
    scrollSortItem.setPreferredSize(new Dimension(150, 200));
    scrollSortItem.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
    panelSortDescription.add(scrollSortItem, constraints);
    constraints.gridy++;
    toolbarButtons = new JToolBar(SwingConstants.HORIZONTAL);
    toolbarButtons.setFloatable(false);
    buttonSortUp = Utilities.createJButton(
        "gnome-go-up.png", EnumImageSize.NORMAL, GT._("Up"), false, null);
    buttonSortUp.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionSortMoveUp"));
    toolbarButtons.add(buttonSortUp);
    buttonSortDown = Utilities.createJButton(
        "gnome-go-down.png", EnumImageSize.NORMAL, GT._("Down"), false, null);
    buttonSortDown.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionSortMoveDown"));
    toolbarButtons.add(buttonSortDown);
    constraints.fill = GridBagConstraints.HORIZONTAL;
    constraints.weighty = 0;
    panelSortDescription.add(toolbarButtons, constraints);
    constraints.gridy++;
    if (modelSort.getSize() > 0) {
      listSort.setSelectedIndex(0);
    }

    // Adding panels
    constraints.fill = GridBagConstraints.BOTH;
    constraints.gridheight = 1;
    constraints.gridwidth = 1;
    constraints.gridx = 0;
    constraints.gridy = 0;
    constraints.weightx = 1;
    constraints.weighty = 1;
    add(panelSortOrders, constraints);
    constraints.gridx++;
    add(panelSortDescription, constraints);
  }

  /**
   * Restore all options to their default values.
   */
  @Override
  public void defaultValues() {
    // Sorting orders
    modelSort.clear();
    List<CompositeComparator<Page>> comparators = PageComparator.getDefaultComparators();
    for (CompositeComparator<Page> comparator : comparators) {
      modelSort.addElement(comparator);
    }
  }

  /**
   * Apply new values to the options.
   */
  @SuppressWarnings("unchecked")
  @Override
  public void apply() {
    // Sorting orders
    List<CompositeComparator<Page>> comparators = new LinkedList<CompositeComparator<Page>>();
    for (int i = modelSort.getSize(); i > 0; i--) {
      Object sort = modelSort.get(i - 1);
      if (sort instanceof CompositeComparator) {
        comparators.add(0, (CompositeComparator<Page>) sort);
      }
    }
    PageComparator.setComparators(comparators);
  }

  /**
   * Action called when Sort Add button is pressed.
   */
  public void actionSortAdd() {
    String name = Utilities.askForValue(this.getParent(), "Input name :", "", null);
    if (name != null) {
      CompositeComparator<Page> comparator = PageComparator.createComparator(name);
      modelSort.addElement(comparator);
      listSort.setSelectedIndex(modelSort.size() - 1);
    }
  }

  /**
   * Action called when Sort Delete button is pressed.
   */
  public void actionSortDelete() {
    int selected = listSort.getSelectedIndex();
    if (selected != -1) {
      modelSort.remove(selected);
      if (selected < modelSort.size()) {
        listSort.setSelectedIndex(selected);
      } else if (selected > 0) {
        listSort.setSelectedIndex(selected - 1);
      }
    }
  }

  /**
   * Action called when Sort Up button is pressed.
   */
  public void actionSortMoveUp() {
    actionSortMove(true);
  }

  /**
   * Action called when Sort Down button is pressed.
   */
  public void actionSortMoveDown() {
    actionSortMove(false);
  }

  /**
   * Action called when Sort Up / Down button is pressed.
   * @param up Flag indicating if it's the Up button.
   */
  private void actionSortMove(boolean up) {
    CompositeComparator<Page> selectedSort = listSort.getSelectedValue();
    NamedComparator<Page> selectedItem = listSortItem.getSelectedValue();
    if ((selectedSort != null) &&
        (selectedItem != null)) {
      CompositeComparator<Page> comparators = selectedSort;
      NamedComparator<Page> comparator = selectedItem;
      comparators.moveComparator(comparator.getName(), up);
      int selected = listSortItem.getSelectedIndex();
      selected += up ? -1 : 1;
      modelSortItem.clear();
      for (int i = 0; i < comparators.getComparatorsCount(); i++) {
        NamedComparator<Page> item = comparators.getComparator(i);
        modelSortItem.addElement(item);
      }
      listSortItem.setSelectedIndex(Math.min(Math.max(0, selected), modelSortItem.size() - 1));
    }
  }

  /* ====================================================================== */
  /* ListSelectionListener implementation                                   */
  /* ====================================================================== */

  /* (non-Javadoc)
   * @see javax.swing.event.ListSelectionListener#valueChanged(javax.swing.event.ListSelectionEvent)
   */
  @SuppressWarnings("unchecked")
  @Override
  public void valueChanged(ListSelectionEvent e) {
    if ((e == null) || (e.getSource() == null)) {
      return;
    }
    if (e.getSource() == listSort) {
      Object value = listSort.getSelectedValue();
      if (value instanceof CompositeComparator) {
        modelSortItem.clear();
        CompositeComparator<Page> comparator = (CompositeComparator<Page>) value;
        for (int i = 0; i < comparator.getComparatorsCount(); i++) {
          NamedComparator<Page> item = comparator.getComparator(i);
          modelSortItem.addElement(item);
        }
      }
    }
  }
}
