:- dynamic(course/7).
:- dynamic(room/5).
:- dynamic(instructor/6).
:- dynamic(student/3).


% rooms

% id-capacity-equipment-availability
room(room1, 10, whiteboard,8,12).
room(room2, 20, whiteboard,9,15).
room(room3, 30, smartboard,12,17).

% courses

% id-instructor-capacity-availability-equipment-room

course(course1, instructor1, 10, 9,10,whiteboard,room1).
course(course2, instructor2, 20, 9,15,projector,room2).
course(course3, instructor3, 30, 12,17,smartboard,room3).

% instructors

% id-courses-equipment

instructor(instructor1, course1, whiteboard).
instructor(instructor2, course2, projector).
instructor(instructor3, course3, smartboard).

% students

% id-courses-handicapped

student(student1, [course1,course3], false).
student(student2, [course2,course3], true).
student(student3, [course3], false).


% Check conflict between two courses
% check_conflict(+Course1, +Course2)
part_1(Course1, Course2) :-
    course(Course1, _, _, Start1, End1, _, Room1),
    course(Course2, _, _, Start2, End2, _, Room2),
    Room1 == Room2,
    Start1 =< Start2,
    End1 >= Start2,
    write('Course '), write(Course1), write(' and course '), write(Course2), write(' conflict'), nl.


% Check which room can be assigned to a given class.
% check_room_class(+Room, +Course)
part_2(Room, Course) :-
    room(Room, Capacity1, Equipment1, Start1, End1),
    course(Course, _, Capacity2, Start2, End2,Equipment2,_),
    Start1 =< Start2,
    End1 >= End2,
    Capacity1 >= Capacity2,
    Equipment1 == Equipment2,
    write('Room '), write(Room), write(' is available for course '), write(Course), nl.

% Check which room can be assigned to which classes
% check_rooms_class(+Course)
part_3(Course,Rooms) :-
    course(Course, _, Capacity1, Start1, End1,Equipment1,_),
    findall(Room, part_2(Room,Course), Rooms).




% Check whether a student can be enrolled to a given class.
% check_students_class(+Student,+Room)
part_4(Student,Room) :-
    student(Student, _, Handicapped),
    room(Room, _, Equipment, _, _),
    Handicapped == true,
    Equipment == projector,   
    write('Student '), write(Student), write(' can be assigned to room '), write(Room), nl.
part_4(Student,Room) :-
    student(Student, _, Handicapped),
    room(Room, _, Equipment, _, _),
    Handicapped == true,
    Equipment == smartboard, 
    write('Student '), write(Student), write(' can be assigned to room '), write(Room), nl.
part_4(Student,Room) :-
    student(Student, _, Handicapped),
    room(Room, _, _, _, _),
    Handicapped == false,
    write('Student '), write(Student), write(' can be assigned to room '), write(Room), nl.

% Check which classes a student can be assigned.
% check_students_class(+Student)
part_5(Student,Rooms) :-
    student(Student, _, Handicapped),
    findall(Room, part_4(Student,Room), Rooms).











% add a course
% add_course(+Course, +Instructor, +Capacity, +Start, +End, +Equipment, +Room)
add_course(Course, Instructor, Capacity, Start, End, Equipment, Room) :-
    assert(course(Course, Instructor, Capacity, Start, End, Equipment, Room)).

% add a room
% add_room(+Room, +Capacity, +Equipment, +Start, +End)
add_room(Room, Capacity, Equipment, Start, End) :-
    assert(room(Room, Capacity, Equipment, Start, End)).
% add a student
% add_student(+Student, +Courses, +Handicapped)
add_student(Student, Courses, Handicapped) :-
    assert(student(Student, Courses, Handicapped)).
% add an instructor
% add_instructor(+Instructor, +Courses, +Equipment)
add_instructor(Instructor, Courses, Equipment) :-
    assert(instructor(Instructor, Courses, Equipment)).


