interface
		integer module function getMinNonEmptyTextBoxSizeFromStart(textBox) result(k)
			implicit none
			integer,dimension(:,:),allocatable,intent(in)::textBox
		end function

		integer module function getMinNonEmptyTextBoxSizeFrom(textBox,j) result(k)
			implicit none
			integer,intent(in)::j
			integer,dimension(:,:),allocatable,intent(in)::textBox
		end function

		integer module function getMaxNonEmptyTextBox(textBox) result(k)
			use appgraphics
			implicit none
			integer::i
			integer,dimension(:,:),allocatable,intent(in)::textBox
		end function

		logical module function isTextBoxRowFull(textBox,i) result(logic)
			implicit none
			integer,dimension(:,:),allocatable,intent(in)::textBox
			integer,intent(in)::i
		end function

		logical module recursive function isTextBoxRowFullUpToColumn(textBox,i,j) result(logic)
			implicit none
			character(10)::dummychar
			integer,dimension(:,:),allocatable,intent(in)::textBox
			integer,intent(in)::i,j
		end function

		logical module function isTextBoxRowEmpty(textBox,i) result(logic)
			implicit none
			integer,dimension(:,:),allocatable,intent(in)::textBox
			integer,intent(in)::i
		end function

		logical module recursive function isTextBoxRowEmptyUpToColumn(textBox,i,j) result(logic)
			implicit none
			character(10)::dummychar
			integer,dimension(:,:),allocatable,intent(in)::textBox
			integer,intent(in)::i,j
		end function

end interface

