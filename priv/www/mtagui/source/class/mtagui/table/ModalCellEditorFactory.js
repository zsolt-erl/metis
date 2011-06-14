qx.Class.define("mtagui.table.ModalCellEditorFactory",
{
  extend : qx.core.Object,
  implement : qx.ui.table.ICellEditorFactory,

  members :
  {
    // overridden
    createCellEditor : function(cellInfo)
    {
      // Create the cell editor window, since we need to return it
      // immediately.
      var cellEditor = new qx.ui.window.Window("Cell Editor");
      cellEditor.setLayout(new qx.ui.layout.VBox(4));
      cellEditor.set(
      {
        padding: 3,
        modal: true,
        showClose: false,
        showMaximize: false,
        showMinimize: false
      });
      cellEditor.moveTo(300, 200);

      // Create a text field in which to edit the data
      cellEditor.__cellEditor = new qx.ui.form.TextArea(cellInfo.value).set(
      {
	  allowGrowY: false,
	  width: 500,
	  height: 200
      });
      cellEditor.add(cellEditor.__cellEditor);

      // Create the "Save" button to close the cell editor
      var closeBtn = new qx.ui.form.Button("Close");
      closeBtn.addListener("execute", function(e) {
        cellEditor.close();
      });
      cellEditor.add(closeBtn);

      // Let them press Enter from the cell editor text field to finish.
      var command = new qx.ui.core.Command("Enter");
      command.addListener("execute", function(e)
      {
	  closeBtn.execute();
          command.dispose();
          command = null;
      });

      return cellEditor;
    },

    // overridden
    getCellEditorValue : function(cellEditor)
    {
      // Return the value in the text field
      return cellEditor.__cellEditor.getValue();
    }
  }
});